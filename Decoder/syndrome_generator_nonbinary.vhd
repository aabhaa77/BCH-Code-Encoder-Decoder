library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.ALL;

entity syndrome_generator_nonbinary is
    generic (
        m : integer := 3;
        n : integer := 15;
        t : integer := 1;
        primitive_polynomial : std_logic_vector(3 downto 0) := "1011"
    );
    port (
        clk: in std_logic;
        reset: in std_logic;
        r_in: in std_logic_vector(n * m - 1 downto 0);
        s_out: out std_logic_vector(2 * t * m - 1 downto 0)
--		  fpga_output: out std_logic_vector(2*t*m - 1 downto 0)
    );
end entity syndrome_generator_nonbinary;

architecture fsm of syndrome_generator_nonbinary is
    type state_type is (RESET_STATE, LOAD_INPUT, EXPONENTIATION, MULTIPLICATION, XOR_SYNDROME, UPDATE_SYNDROME, DONE);
    signal current_state, next_state : state_type;

--	 signal r_in : std_logic_vector(n * m - 1 downto 0) := "000000000000000100010001000100010001000100010001000000000001";
	 
    signal syndromes : std_logic_vector(m * 2 * t - 1 downto 0);
    signal current_symbol, current_result, product : std_logic_vector(m - 1 downto 0);
    signal current_syndrome : std_logic_vector(m - 1 downto 0) := (others => '0');
    signal synd_count, j_count : integer := 0;
 
--------------------------------------------------------------------------------------------------------------------------------------   

function mod_reduction(
        poly: std_logic_vector(2 * M - 1 downto 0);
        primitive_polynomial: std_logic_vector(M downto 0) := "1011"
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

------------------------------------------------------------------------------------------------------------------------------
function gf_pow(
	alpha : std_logic_vector(m-1 downto 0);
	exp : integer
	) return std_logic_vector is
	variable result : std_logic_vector(m-1 downto 0) := "001";
	variable i : integer := 0;
	begin
		for i in 0 to (2 * t * n - 1) loop
			if (i < exp) then
				result := gf_mul(result, alpha);
			end if;
		end loop;
	return result;

end function;
--------------------------------------------------------------------------------------------------------------------------------------------
begin

    process(clk, reset)
    begin
        if reset = '1' then
            current_state <= RESET_STATE;
        elsif rising_edge(clk) then
            current_state <= next_state;
        end if;
    end process;

	 process(current_state, j_count, synd_count)
begin
    case current_state is
        when RESET_STATE =>
            if synd_count < (2 * t) then
                next_state <= LOAD_INPUT;
            else
                next_state <= DONE;
            end if;

        when LOAD_INPUT =>
            if j_count < n then
                next_state <= EXPONENTIATION;
            else
                next_state <= UPDATE_SYNDROME;
            end if;

        when EXPONENTIATION =>
            next_state <= MULTIPLICATION;

        when MULTIPLICATION =>
            next_state <= XOR_SYNDROME;

        when XOR_SYNDROME =>
            if j_count = n - 1 then
                next_state <= UPDATE_SYNDROME;
            else
                next_state <= LOAD_INPUT;
            end if;

        when UPDATE_SYNDROME =>
            if synd_count < (2 * t) - 1 then
                next_state <= LOAD_INPUT;
            else
                next_state <= DONE;
            end if;

        when DONE =>
            next_state <= DONE;

        when others =>
            next_state <= RESET_STATE;
    end case;
end process;

process(clk, reset)
begin
    if reset = '1' then
        syndromes <= (others => '0');
        synd_count <= 0;
        j_count <= 0;
        current_syndrome <= (others => '0');
        s_out <= (others => '0');
    elsif rising_edge(clk) then
        case current_state is
            when RESET_STATE =>
                syndromes <= (others => '0');
                synd_count <= 0;
                j_count <= 0;
                current_syndrome <= (others => '0');
                s_out <= (others => '0');
                
            when LOAD_INPUT =>
                current_symbol <= r_in((n - j_count) * m - 1 downto (n - j_count - 1) * m);

            when EXPONENTIATION =>
                current_result <= gf_pow("010", ((synd_count + 1) * (j_count)));

            when MULTIPLICATION =>
                product <= gf_mul(current_symbol, current_result);

            when XOR_SYNDROME =>
                current_syndrome <= current_syndrome xor product;
                j_count <= j_count + 1;

            when UPDATE_SYNDROME =>
                syndromes((synd_count + 1) * m - 1 downto (synd_count )* m) <= current_syndrome;
                synd_count <= synd_count + 1;
                current_syndrome <= (others => '0');
                j_count <= 0;

            when DONE =>
                s_out <= syndromes;

--           Negation for SSD Testing on DE10 Board
--					 fpga_output (15 downto 10) <= syndromes(15 downto 10);
--					 fpga_output (9 downto 0) <= syndromes(9 downto 0);

        end case;
    end if;
end process;

end architecture fsm;
