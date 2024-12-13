library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity forney_algorithm is
    generic (
        n : integer := 15;
        m : integer := 4;      
        k : integer := 7;
        t : integer := 2      -- Error-correcting capability
    );
    Port (
        clk               : in  std_logic;
        reset             : in  std_logic;
        start             : in  std_logic;
--        syndromes         : in  std_logic_vector((2 * t * m) - 1 downto 0);
--        error_locator     : in  std_logic_vector((2 * t * m) - 1 downto 0);
--        error_positions   : in  std_logic_vector(n - 1 downto 0);
        error_magnitudes  : out std_logic_vector((n * m) - 1 downto 0);
--		  fpga_output : out std_logic_vector((n* M) - 1 downto 0);

        done              : out std_logic
    );
end forney_algorithm;

architecture Behavioral of forney_algorithm is

    -- FSM States
    type state_type is (IDLE, COMPUTE_EVALUATOR, COMPUTE_DERIVATIVE, CALCULATE_MAGNITUDES, DONE_STATE);
    signal state : state_type := IDLE;
	 
	 signal syndromes         :  std_logic_vector((2 * t * m) - 1 downto 0) := "0100001100100101";
    signal  error_locator    :  std_logic_vector((2 * t * m) - 1 downto 0) := "0000101001000001";
    signal  error_positions   : std_logic_vector(n - 1 downto 0) := "001001000000000";

    -- Signals
    signal omega, lambda_prime : std_logic_vector((2 * t * m) - 1 downto 0);
    signal error_magnitude_array : std_logic_vector((n * m) - 1 downto 0) := (others => '0');
    signal index : integer := 0;

    -- Lookup table and helper functions for finite field operations
--    type inv_lookup_type is array(0 to 3) of std_logic_vector(M - 1 downto 0);
--    constant inv_lookup : inv_lookup_type := (
--        "00",  -- Inverse of 01
--        "01",  -- Inverse of 01
--        "11",  -- Inverse of 10
--        "10"   -- Inverse of 11
--    );

--    type inv_lookup_type is array(0 to 7) of std_logic_vector(M - 1 downto 0);
--    constant inv_lookup : inv_lookup_type := (
--        "000",  -- Inverse of 01
--        "001",  -- Inverse of 01
--		  "101",  -- Inverse of 10
--		  "110",   -- Inverse of 11
--		  "111",
--		  "010",
--		  "011",
--		  "100"
--    );

		type inv_lookup_type is array(0 to 15) of std_logic_vector(M - 1 downto 0);
		 constant inv_lookup : inv_lookup_type := (
			  "0000",  -- Inverse of 01
			  "0001",  -- Inverse of 01
			  "1001",  -- Inverse of 10
			  "1110",   -- Inverse of 11
			  "1101",
			  "1011",
			  "0111",
			  "0110",
			  "1111",
			  "0010",
			  "1100",
			  "0101",
			  "1010",
			  "0100",
			  "0011",
			  "1000"
		 );
    
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
	
	function gf_mul(a : std_logic_vector(m-1 downto 0);
      b : std_logic_vector(m-1 downto 0))
		return std_logic_vector is

		variable A1 : std_logic_vector(2*m - 1 downto 0) := (others => '0');
		variable A2 : std_logic_vector(2*m - 1 downto 0);
		variable A3 : std_logic_vector(2*m - 1 downto 0);
		variable product : std_logic_vector(2*m - 1 downto 0);
		variable result : std_logic_vector(m - 1 downto 0);			
	
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
   
	function gf_pow(
		alpha : std_logic_vector(m-1 downto 0);
		exp : integer
		) return std_logic_vector is
		variable result : std_logic_vector(m-1 downto 0) := "0001";
		variable i : integer := 0;
		begin
			for i in 0 to (2 * t * n - 1) loop
				if (i < exp) then
					result := gf_mul(result, alpha);
				end if;
			end loop;
		return result;

	end function;
	
	function compute_error_evaluator(
		syndromes_in : std_logic_vector((2 * t * m) - 1 downto 0);
      locator : std_logic_vector((2 * t * m) - 1 downto 0)
		) return std_logic_vector is
		
		variable temp_evaluator : std_logic_vector((2 * t * m) - 1 downto 0) := (others => '0');
      variable synd : std_logic_vector(m - 1 downto 0);
      variable loc : std_logic_vector(m - 1 downto 0);
		variable product : std_logic_vector(m - 1 downto 0);
		variable xored : std_logic_vector(m - 1 downto 0);
		variable evaluator : std_logic_vector((2 * t * m) - 1 downto 0);

		begin
			for i in 0 to (2 * t - 1) loop
				synd := syndromes_in((i + 1) * m - 1 downto i * m);
				for j in 0 to (2 * t - 1) loop
					if (i + j) < 2 * t then
						loc := locator((j + 1) * m - 1 downto j * m);
						product := gf_mul(synd, loc);
						xored := temp_evaluator((i + j + 1) * m - 1 downto (i + j) * m) xor product;
						temp_evaluator((i + j + 1) * m - 1 downto (i + j) * m) := xored;
					end if;
			   end loop;
		  end loop;

		 evaluator := temp_evaluator;
		 
		 return evaluator;
	 end function;


	function compute_locator_derivative(
		locator : std_logic_vector((2 * t * m) - 1 downto 0)
		) return std_logic_vector is
		variable derivative : std_logic_vector((2 * t * m) - 1 downto 0) := (others => '0');

		begin
			for i in 1 to (2 * t) - 1 loop
				if (i mod 2) = 1 then
					derivative((i) * m - 1 downto (i - 1) * m) := locator((i + 1) * m - 1 downto i * m);
				else
					derivative((i) * m - 1 downto (i - 1) * m) := (others => '0');
				end if;
			end loop;

		return derivative;
	end function;
	
	function evaluate_poly(
		poly : std_logic_vector((2 * t * m) - 1 downto 0);
		value_1 : std_logic_vector(m - 1 downto 0)
		) return std_logic_vector is
    
		variable result : std_logic_vector(m - 1 downto 0) := (others => '0');
		variable current_power : std_logic_vector(m - 1 downto 0) := "0001"; -- Start with alpha^0 (1)
		variable coeff : std_logic_vector(m - 1 downto 0);
		
		begin
--			current_power(0) := '1';
			
			for i in 0 to (2 * t) - 1 loop
				coeff := poly((i + 1) * m - 1 downto i * m);
				result := result xor gf_mul(coeff, current_power);
				current_power := gf_mul(current_power, value_1);
			end loop;

		return result;
	end function;



	begin

    -- FSM Process
    process(clk, reset)
        variable err_pos, err_inv, num, den, den_inv, error_mag : std_logic_vector(m - 1 downto 0);
    begin
        if reset = '1' then
            state <= IDLE;
            error_magnitudes <= (others => '0');
            done <= '0';
            index <= 0;
        elsif rising_edge(clk) then
            case state is
                when IDLE =>
                    if start = '1' then
                        state <= COMPUTE_EVALUATOR;
                        error_magnitude_array <= (others => '0');
                        done <= '0';
                    end if;

                -- Compute the error evaluator polynomial
                when COMPUTE_EVALUATOR =>
                    omega <= compute_error_evaluator(syndromes, error_locator);
                    state <= COMPUTE_DERIVATIVE;

                -- Compute the derivative of the error locator polynomial
                when COMPUTE_DERIVATIVE =>
                    lambda_prime <= compute_locator_derivative(error_locator);
                    index <= 0; -- Reset index for error magnitude calculation
                    state <= CALCULATE_MAGNITUDES;

                -- Calculate error magnitudes
                when CALCULATE_MAGNITUDES =>
                    if index < n then
                        if error_positions(index) = '1' then
                            err_pos := gf_pow("0010", index); -- Determine power of alpha
                            err_inv := inv_lookup(to_integer(unsigned(err_pos)));
									 num := evaluate_poly(omega, err_inv);
                            den := evaluate_poly(lambda_prime, err_inv);

                            den_inv := inv_lookup(to_integer(unsigned(den)));
                            error_mag := gf_mul(num, den_inv);

                            error_magnitude_array(index * m + m - 1 downto index * m) <= error_mag;
                        end if;
                        index <= index + 1;
                    else
                        state <= DONE_STATE;
                    end if;

                when DONE_STATE =>
                    error_magnitudes <= error_magnitude_array;

                    done <= '1';
                    
            end case;
        end if;
    end process;
end Behavioral;
