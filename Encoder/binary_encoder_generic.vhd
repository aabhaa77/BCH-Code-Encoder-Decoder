library ieee;
use ieee.std_logic_1164.ALL;
use ieee.std_logic_arith.ALL;
use ieee.std_logic_unsigned.ALL;

entity binary_encoder_generic is
    generic (
        n : integer := 15;  -- Codeword length
        k : integer := 11  -- Message length
    );
    port (
        clk: in  std_logic;
        reset: in  std_logic;
        d_in: in  std_logic_vector(k - 1 downto 0);  --message input of length k
        start: in  std_logic;
		  generator: in  std_logic_vector((n - k) downto 0);  
        remainder: out std_logic_vector((n - k - 1) downto 0);   -- Remainder of length n-k
        done: out std_logic
    );
end binary_encoder_generic;

architecture Behavioral of binary_encoder_generic is
    signal lfsr: std_logic_vector((n - k - 1) downto 0) := (others => '0');  -- (n-k) bit LFSR contents
    signal count: integer range 0 to n := 0;  --count processed bits
    signal shift_reg : std_logic_vector((n - 1) downto 0);  -- 15-bit message including 4 parity bits
	 signal zero_vector: std_logic_vector((n - k - 1) downto 0) := (others => '0');

	 begin
		process(clk, reset)
		begin
        if reset = '1' then
            lfsr <= (others => '0');  			-- Reset the LFSR
            shift_reg <= (others => '0');  	-- Clear the shift register
            count <= 0;
            done <= '0';
        elsif rising_edge(clk) then
            if start = '1' then
                --Load the message into the shift register (shifted by 4 to multiply by x^4)
                shift_reg <= d_in & zero_vector;  	-- Append n-k zeros (for parity bits)
                lfsr <= (others => '0');  					-- initial parity values
                count <= 0;
                done <= '0';
            
				elsif count < n then
				
                if lfsr(n - k - 1) = '1' then
                    lfsr <= (lfsr((n - k - 2) downto 0) & shift_reg(n - 1)) xor generator((n - k - 1) downto 0);
                else
                    lfsr <= lfsr((n - k - 2) downto 0) & shift_reg(n - 1);
                end if;
					 
                shift_reg <= shift_reg((n - 2) downto 0) & '0';
					 count <= count + 1;

                if count = n - 1 then
                    done <= '1';
                end if;
            end if;
        end if;
    end process;

    remainder <= lfsr;

end Behavioral;
