library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity NB_ENCODER is
		generic(
			k : integer := 7; -- No. of input symbols
			m : integer := 4; -- No. of bits of primitive polynomial
			n : integer := 15); -- Length of codeword

    port (
        dividend : in std_logic_vector(k * (m ) - 1 downto 0); -- input
--        divisor  : in std_logic_vector((n - k + 1) * (m) - 1 downto 0); -- generator
        remainder_out : out std_logic_vector((n - k) * (m) - 1 downto 0) -- Remainder
    );
end NB_ENCODER;

architecture Behavioral of NB_ENCODER is
	type INTEGERARRAY is array (natural range <>, natural range <>) of integer;
	signal quotient : INTEGERARRAY(0 to 0, 0 to n - 1);
	signal dividend_int : INTEGERARRAY(0 to 0, 0 to n - 1);
	signal divisor_int : INTEGERARRAY(0 to 0, 0 to n - k);
	signal remainder_int : INTEGERARRAY(0 to 0, 0 to n - k);
	
	signal divisor  : std_logic_vector((n - k + 1) * (m) - 1 downto 0) := "000100010001000000010000000000000001"; -- generator



function mod_reduction(
        poly: std_logic_vector(2 * M - 1 downto 0);
        primitive_polynomial: std_logic_vector(M downto 0) := "10011"
    ) return std_logic_vector is
        variable reduced : std_logic_vector(2 * M - 1 downto 0) := poly;
        variable lfsr : std_logic_vector(M downto 0) := (others => '0');
        variable temp : std_logic_vector(M downto 0) := (others => '0');
		  variable zero : std_logic_vector(M downto 0) := (others => '0');
		  
		  begin
		   lfsr := poly(2 * M - 1 downto (M - 1));
			reduced := poly(M - 2 downto 0) & zero;
			for i in 0 to (M) - 1 loop
				if (i < M - 1) then
					if lfsr(M) = '1' then
						 temp := lfsr xor primitive_polynomial;
						 lfsr := temp((M - 1) downto 0) & reduced(2 * M - 1);
					else
						 lfsr := lfsr((M - 1) downto 0) & reduced(2 * M - 1);
					end if;
				end if;
				if (i = M - 1) then
					if lfsr(M) = '1' then
						 temp := lfsr xor primitive_polynomial;
						 lfsr := temp;
					end if;
            end if;
            reduced := reduced((2 * M - 2) downto 0) & '0';
        end loop;
        return lfsr(M-1 downto 0);
	end function;
-------------------------------------------------------------------------------------------------------------------------------------
 
function gf_mul(a : std_logic_vector(m-1 downto 0);
                 b : std_logic_vector(m-1 downto 0))
                 return std_logic_vector is
	variable A1 : std_logic_vector(2*m - 1 downto 0) := (others => '0');
	variable A2 : std_logic_vector(2*m - 1 downto 0);
	variable A3 : std_logic_vector(2*m - 1 downto 0);
	variable product : std_logic_vector(2*m - 1 downto 0);
	variable result : std_logic_vector(m-1 downto 0);

	begin
		A2 := (others => '0');				
		A3 := (others => '0');
		A1(m-1 downto 0) := a(m-1 downto 0);

		for i in 0 to m-1 loop
			if b(i) = '1' then 		
				A2 := A2 XOR A1;		
			else 							
				A2 := A2;				
			end if;
			A3(2 * m - 1 downto 1) := A1(2*m - 2 downto 0);  				
			A3(0) := A1(2 * m - 1);															
			A1 := A3;																					
		end loop;
		
		product := A2;
		
		result := mod_reduction(product);

    return result;
end function;
--------------------------------------------------------------------------------------------------------------------------------------

function gf_inv(a : integer; b : integer) return integer is
    variable c : integer := 0;
	begin
    if a = 0 then
        return 0;
    end if;

    for c in 1 to 15 loop
        if (gf_mul(std_logic_vector(to_unsigned(a, m)), std_logic_vector(to_unsigned(c, m)))) = std_logic_vector(to_unsigned(b, m)) then
            return c;
        end if;
    end loop;

    return 0;
end gf_inv;

---------------------------------------------------------------------------------------------------------

begin

	 process(dividend)
		variable div : INTEGERARRAY(0 to 0, 0 to n - 1);
		variable A : std_logic_vector(m - 1 downto 0);  
		begin

		for j in 0 to k-1 loop 
			A := dividend(((k - j) * (m) - 1) downto ((k - j - 1) * (m))); 
			div(0, j) := to_integer(unsigned(A));
		end loop;
		
		for j in k to n - 1 loop
			div(0, j) := 0;
		end loop;
		
		dividend_int <= div;
		
		end process;
		
		
		process(divisor)
			variable div2 : INTEGERARRAY(0 to 0, 0 to n-k);
			variable temp2 : std_logic_vector(m - 1 downto 0);  
			begin

			for j in 0 to (n - k) loop
			  temp2 := divisor(((n - k + 1 - j) * (m) - 1) downto ((n - k - j) * (m))); 
			  div2(0, j) := to_integer(unsigned(temp2));
		 end loop;

		divisor_int <= div2;
		
		end process;
--    
	 process(dividend_int, divisor_int)
        variable temp_dividend : INTEGERARRAY(0 to 0, 0 to n - 1); -- Copy of dividend for manipulation
        variable temp_quotient : INTEGERARRAY(0 to 0, 0 to n - 1); -- Resulting quotient
        variable lead_coeff_inv : integer; -- Inverse of leading coefficient of divisor
        variable scale_factor : integer;

		  begin
				for i in 0 to n - 1 loop
					temp_dividend(0, i) := dividend_int(0, i);
			   end loop;
			  
			  temp_quotient := (others => ( others => 0));

			  for i in 0 to k - 1 loop
					if temp_dividend(0, i) /= 0 then
						scale_factor := gf_inv(divisor_int(0, 0), temp_dividend(0, i));
						temp_quotient(0, i) := scale_factor;

                -- Subtract scaled divisor from temp_dividend
						 for j in 0 to n - k loop
							  temp_dividend(0, i + j) := to_integer(unsigned(std_logic_vector(to_unsigned(temp_dividend(0, i + j), m)) xor gf_mul((std_logic_vector(to_unsigned(scale_factor, m))), std_logic_vector(to_unsigned(divisor_int(0, j), m)))));
						 end loop;
					end if;
				end loop;

        -- Assign the quotient and remainder
        quotient <= temp_quotient;
        -- Assign the remainder from temp_dividend to remainder_int using a loop
			for i in 0 to n - k loop
				 remainder_int(0, i) <= temp_dividend(0, k - 1 + i);
			end loop;

    end process;
	 
		process(remainder_int)
			begin
			
			for i in 1 to n - k  loop
				remainder_out(((n - k - i + 1) * (m) - 1) downto ((n - k - i) * m) ) <= std_logic_vector(to_unsigned(remainder_int(0, i), m));
			end loop;
			
			
		end process;
		
end Behavioral;
