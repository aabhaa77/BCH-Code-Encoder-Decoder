library ieee;
use ieee.std_logic_1164.ALL;
use ieee.std_logic_arith.ALL;
use ieee.std_logic_unsigned.all;

entity LFSR_Polydiv is
    Port (
        clk     : in  std_logic;
        reset   : in  std_logic;
        data_in : in  std_logic;  -- Single bit of the input message
        enable  : in  std_logic;  -- Enable signal to shift
        crc_out : out std_logic_vector(3 downto 0)  -- 4-bit remainder (LFSR)
    );
end LFSR_Polydiv;

architecture behavioral of LFSR_Polydiv is
    signal lfsr : std_logic_vector(3 downto 0) := (others => '0');  -- 4-bit LFSR
    constant generator : std_logic_vector(4 downto 0) := "10011";  -- Generator polynomial for BCH(15,11)
begin

    process(clk, reset)
    begin
        if reset = '1' then
            lfsr <= (others => '0');  -- Reset the LFSR to all zeros
        elsif rising_edge(clk) then
            if enable = '1' then
                -- Shift the LFSR and bring in the new bit (data_in)
                -- Check if MSB is '1' (meaning we need to XOR with the generator polynomial)
                if lfsr(3) = '1' then
                    -- Shift and XOR with generator polynomial
                    lfsr <= (lfsr(2) & lfsr(1) & lfsr(0) & data_in) xor generator(3 downto 0);
                else
                    -- Simply shift the LFSR
                    lfsr <= lfsr(2 downto 0) & data_in;
                end if;
            end if;
        end if;
    end process;

    -- Output the LFSR contents (this is the remainder of the division)
    crc_out <= lfsr;

end behavioral;
