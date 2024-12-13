--Generic field generator code
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- Generic Field Generator for GF(2^m)
entity field_generator is
    generic (
        m : integer := 5;  -- Degree of the field, GF(2^m)
        generator_polynomial : std_logic_vector(5 downto 0) := "100100"  -- Generator polynomial
    );
    port (
        clk: in std_logic;
        reset: in  std_logic;
        d_out: out std_logic_vector(m - 1 downto 0)
    );
end entity; 

architecture behavioral of field_generator is
    signal output: std_logic_vector(m - 1 downto 0);
begin
    process(clk, reset)
    begin
        if reset = '1' then
            -- Initialize the output to a default value (e.g., "1000" for GF(4)).
            output <= (others => '0');
            output(m - 1) <= '1';          -- Set the MSB to 1 to begin with a non-zero field element
        elsif rising_edge(clk) then
            output(m - 1) <= output(0);
            
            -- Apply XOR for bits as defined by the polynomial (loop through the polynomial).
            for i in 0 to m-2 loop
                output(i) <= output(i + 1) xor (output(0) and generator_polynomial(i + 1));
            end loop;
            d_out <= output;
        end if;
    end process;
end behavioral;
