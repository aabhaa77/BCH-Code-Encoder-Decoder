library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity berlekamp_massey is
    generic (
        N : integer := 15;
        M : integer := 3;
		  t : integer := 1
    );
    Port (
        clk          : in  std_logic;
        reset        : in  std_logic;
        start        : in  std_logic;
        syndromes    : in  std_logic_vector((2 * t * M) - 1 downto 0);
        c_out        : out std_logic_vector((2 * t * M) - 1 downto 0);
--		  fpga_output : out std_logic_vector((2 * t * M) - 1 downto 0);
        done         : out std_logic
    );
end berlekamp_massey;

architecture Behavioral of berlekamp_massey is
    -- Maximum length of the connection polynomial = 2t
    constant L_MAX : integer := 2;

--	 signal  syndromes :std_logic_vector((2 * t * M) - 1 downto 0) := "0101001000110100";

	 
    signal LFSR_len, k, l : integer := 0; 
    signal dm : std_logic_vector(M - 1 downto 0) := (others => '0'); -- Discrepancy multiplier
    signal d : std_logic_vector(M - 1 downto 0) := (others => '0');  -- Discrepancy as a vector
    
    signal c_x : std_logic_vector((L_MAX * M) - 1 downto 0) := (others => '0'); -- Connection polynomial
    signal p_x : std_logic_vector((L_MAX * M) - 1 downto 0) := (others => '0'); -- Previous polynomial
    signal zeroes : std_logic_vector(M - 1 downto 0) := (others => '0');
	 signal flag : std_logic_vector (1 downto 0) := (others => '0');

    -- Syndrome array to hold the individual syndrome values
    type syndrome_array_type is array (0 to 2 * t - 1) of std_logic_vector(M - 1 downto 0);
    signal syndromes_array : syndrome_array_type;

    type state_type is (IDLE, INIT, COMPUTE_DISCREPANCY, WAIT_STATE, UPDATE_POLY, DELAY_STATE, DONE_STATE);
    signal state, next_state : state_type := IDLE;
    
--    type inv_lookup_type is array(0 to 3) of std_logic_vector(M - 1 downto 0);
--    constant inv_lookup : inv_lookup_type := (
--        "00",  -- Inverse of 01
--        "01",  -- Inverse of 01
--		  "11",  -- Inverse of 10
--		  "10"   -- Inverse of 11
--    );

--    type inv_lookup_type is array(0 to 15) of std_logic_vector(M - 1 downto 0);
--    constant inv_lookup : inv_lookup_type := (
--        "0000",  -- Inverse of 01
--        "0001",  -- Inverse of 01
--		  "1001",  -- Inverse of 10
--		  "1110",   -- Inverse of 11
--		  "1101",
--		  "1011",
--		  "0111",
--		  "0110",
--		  "1111",
--		  "0010",
--		  "1100",
--		  "0101",
--		  "1010",
--		  "0100",
--		  "0011",
--		  "1000"
--    );

    type inv_lookup_type is array(0 to 7) of std_logic_vector(M - 1 downto 0);
    constant inv_lookup : inv_lookup_type := (
        "000",  -- Inverse of 01
        "001",  -- Inverse of 01
		  "101",  -- Inverse of 10
		  "110",   -- Inverse of 11
		  "111",
		  "010",
		  "011",
		  "100"
    );

--    type inv_lookup_type is array(0 to 1) of std_logic_vector(M - 1 downto 0);
--    constant inv_lookup : inv_lookup_type := (
--        "0",  -- Inverse of 01
--        "1"  -- Inverse of 01
--
--    );


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
    
begin

	process (clk, reset)
		 variable temp_product : std_logic_vector((L_MAX * M) - 1 downto 0);
		 variable temp_product2 : std_logic_vector((L_MAX * M) - 1 downto 0) := (others => '0');
		 variable syndrome_sum : std_logic_vector(M - 1 downto 0);
		 variable curr_d : std_logic_vector(M - 1 downto 0);
		 variable curr_d2 : std_logic_vector(M - 1 downto 0);
		 variable t_x : std_logic_vector((L_MAX * M) - 1 downto 0);
		 variable ci, curr_synd : std_logic_vector(M - 1 downto 0);
		 variable dm_inv : std_logic_vector(M - 1 downto 0) := (others => '0');
		 variable shifted_temp_product : std_logic_vector((L_MAX * M) - 1 downto 0) := (others => '0');

	 
	begin
		if reset = '1' then
			state <= IDLE;
			c_x <= (others => '0');
         p_x <= (others => '0');
         LFSR_len <= 0;
         k <= 1;
         l <= 1;
         dm <= "001";
         d <= (others => '0');
         done <= '0';
         c_out <= (others => '0');
         syndromes_array <= (others => (others => '0'));
		  
		   temp_product := (others => '0');
		   temp_product2 := (others => '0');
         syndrome_sum := (others => '0');
         t_x := (others => '0');
         ci := (others => '0');
         curr_synd := (others => '0');
		   curr_d := (others => '0');
		   curr_d2 := (others => '0');
		  
    elsif rising_edge(clk) then
		case state is
			when IDLE =>
				if start = '1' then
					state <= INIT;
            else
               state <= IDLE;
            end if;
            
            when INIT =>
					c_x(0) <= '1';
               p_x(0) <= '1';
               LFSR_len <= 0;
               l <= 1;
               k <= 1;
               dm(0) <= '1';

               for i in 0 to 2*t - 1 loop
                   syndromes_array(i) <= syndromes(i * M + M - 1 downto i * M);
               end loop;
					 
               syndrome_sum := syndromes_array(k - 1);
               d <= syndromes(m - 1 downto 0);

               state <= COMPUTE_DISCREPANCY;

            when COMPUTE_DISCREPANCY =>
               if k <= 2 * t then
						syndrome_sum := syndromes_array(k - 1);
                  curr_d := syndrome_sum;
						curr_d2 := syndrome_sum;

                  for i in 1 to L_MAX - 1 loop
							if (i <= LFSR_len) then
								if (i <= k) then
									ci := c_x((i + 1) * M - 1 downto i * M);
                           curr_synd := syndromes_array(k - i - 1);
									curr_d2 := curr_d xor gf_mul(ci, curr_synd);
									curr_d := curr_d2;

								end if;
                     end if;
                  end loop;
						d <= curr_d2;
                  state <= WAIT_STATE;
                else
                  state <= DONE_STATE;
                end if;

            when WAIT_STATE =>
                state <= UPDATE_POLY;
                
            when DELAY_STATE =>
                state <= COMPUTE_DISCREPANCY;
					 
				when UPDATE_POLY =>
					if d = zeroes then
						l <= l + 1;
               else
						if (2 * LFSR_len) >= k then
							flag <= "11";
                     dm_inv := inv_lookup(to_integer(unsigned(dm)));
                     for i in 0 to L_MAX - 1 loop
								temp_product2((i + 1) * M - 1 downto i * M) := gf_mul(dm_inv, p_x((i + 1) * M - 1 downto i * M));
                     end loop;

                     for i in 0 to L_MAX - 1 loop
                        temp_product((i + 1) * M - 1 downto i * M) := gf_mul(temp_product2((i + 1) * M - 1 downto i * M), d);
                     end loop;

                     shifted_temp_product := std_logic_vector(shift_left(unsigned(temp_product), l * M));
                     c_x <= c_x xor shifted_temp_product;
                     l <= l + 1;
                  else
							t_x := c_x;
							flag <= "10";
							dm_inv := inv_lookup(to_integer(unsigned(dm)));	
                     for i in 0 to L_MAX - 1 loop
								temp_product2((i + 1) * M - 1 downto i * M) := gf_mul(dm_inv, p_x((i + 1) * M - 1 downto i * M));
                     end loop;

                     for j in 0 to L_MAX - 1 loop
                        temp_product((j + 1) * M - 1 downto j * M) := gf_mul(d, temp_product2((j + 1) * M - 1 downto j * M));
                     end loop;
                     shifted_temp_product := std_logic_vector(shift_left(unsigned(temp_product), l * M));
                     c_x <= c_x xor shifted_temp_product;
                     p_x <= t_x;
                     LFSR_len <= k - LFSR_len;
                     dm <= d;
                     l <= 1;
                  end if;
               end if;

               k <= k + 1;
               state <= DELAY_STATE;
            
				when DONE_STATE =>
					c_out <= c_x;
--					fpga_output (15) <= not c_x(15);
--					fpga_output (14) <= not c_x(14);
--					fpga_output (13) <= not c_x(13);
--					fpga_output (12) <= not c_x(12);
--					fpga_output (11) <= not c_x(11);
--					fpga_output (10) <= not c_x(10);
--					 fpga_output (9 downto 0) <= c_x(9 downto 0);

               done <= '1';
                
            when others =>
               state <= IDLE;
        end case;
		end if;
	end process;

end Behavioral;
