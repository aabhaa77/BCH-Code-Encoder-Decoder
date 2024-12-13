library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity chien_search is
    generic (
        n : integer := 7;
        t : integer := 2;
        m : integer := 4
    );
    Port (
        clk          : in  std_logic;
        reset        : in  std_logic;
        start        : in  std_logic;
--      c_in         : in  std_logic_vector((2 * t * m) - 1 downto 0);
		  lut_index    : in std_logic_vector(2 downto 0);
        error_pos    : out std_logic_vector(n - 1 downto 0);
		  fpga_output : out std_logic_vector (n - 1 downto 0);
        done         : out std_logic
    );
end chien_search;

architecture Behavioral of chien_search is
    constant ALPHA : std_logic_vector(M - 1 downto 0) := "0010";

    type state_type is (IDLE, INIT, EVALUATE, UPDATE, COMPLETE);
    signal current_state, next_state : state_type;
   
	 signal c_in  : std_logic_vector((2 * t * m) - 1 downto 0) := "0000101001000001";

    signal alpha_pow : std_logic_vector(M - 1 downto 0);
    signal eval_result : std_logic_vector(M - 1 downto 0);
    signal err_pos_temp : std_logic_vector(N - 1 downto 0) := (others => '0');
    signal index : integer := 0;
    signal zeroes : std_logic_vector(M - 1 downto 0) := (others => '0');

--LUT for sample inputs
	 type inv_lookup_type is array(0 to 7) of std_logic_vector(15 downto 0);
    constant inv_lookup : inv_lookup_type := (
		"0000101001000001",
		"0000110001010100",
		"0000001000111100",
		"0001010100101101",
		"0000000000000000",
		"0000000000000000",
		"0000000000000000",
		"0000000000000000");
		
--		type inv_lookup_type is array(0 to 7) of std_logic_vector(7 downto 0);
--    constant inv_lookup : inv_lookup_type := (
--		"110010",
--		"001101",
--		"011010",
--		"100101",
--		"000000",
--		"000000",
--		"000000",
--		"000000");
		

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


    function eval_polynomial(c: std_logic_vector((2*t * m) - 1 downto 0); alpha: std_logic_vector(M - 1 downto 0)) return std_logic_vector is
variable result : std_logic_vector(M - 1 downto 0) := (others => '0');
      variable curr_alpha : std_logic_vector(M - 1 downto 0) := alpha;

begin
result := c(M - 1 downto 0);
for i in 1 to 2*t - 1 loop
result := result xor gf_mul(c((i + 1) * M - 1 downto i * M), curr_alpha);
            curr_alpha := gf_mul(curr_alpha, alpha);
        end loop;
       
return result;
    end function;
   
begin
    process(clk, reset)
    begin
        if reset = '1' then
            current_state <= IDLE;
        elsif rising_edge(clk) then
            current_state <= next_state;
        end if;
    end process;

    process(current_state, start, eval_result, index)
    begin
        case current_state is
            when IDLE =>
                if start = '1' then
                    next_state <= INIT;
                else
                    next_state <= IDLE;
                end if;

            when INIT =>
                next_state <= EVALUATE;

            when EVALUATE =>
                if index < N then
                    next_state <= UPDATE;
                else
                    next_state <= COMPLETE;
                end if;

            when UPDATE =>
                next_state <= EVALUATE;

            when COMPLETE =>

            when others =>
                next_state <= IDLE;
        end case;
    end process;

    process(clk, reset)
    begin
        if reset = '1' then
            error_pos <= (others => '0');
            err_pos_temp <= (others => '0');
            done <= '0';
            alpha_pow <= (others => '0');
            index <= 0;
            eval_result <= (others => '0');
				fpga_output(14 downto 10) <= "11111";
				c_in <= inv_lookup(to_integer(unsigned(lut_index)));
        elsif rising_edge(clk) then
            case current_state is
                when IDLE =>
                    done <= '0';
                    err_pos_temp <= (others => '0');
                    index <= 0;
                    alpha_pow <= "0001";

                when INIT =>
                    alpha_pow <= "0001";
                    eval_result <= eval_polynomial(c_in, alpha_pow);
               
                when EVALUATE =>
                    eval_result <= eval_polynomial(c_in, alpha_pow);

                when UPDATE =>
                    if eval_result = zeroes then
                        err_pos_temp(index) <= '1';
                    end if;
                    alpha_pow <= gf_mul(alpha_pow, ALPHA);
                    index <= index + 1;

                when COMPLETE =>
                    error_pos <= err_pos_temp;
                    done <= '1';

      --Inverted outputs on SSD for testing on De10 Board
			-- fpga_output (14) <= not err_pos_temp(14);
			-- fpga_output (13) <= not err_pos_temp(13);
			-- fpga_output (12) <= not err_pos_temp(12);
			-- fpga_output (11) <= not err_pos_temp(11);
			-- fpga_output (10) <= not err_pos_temp(10);
			-- fpga_output (9 downto 0) <= err_pos_temp(9 downto 0);
 
                when others =>
                    done <= '0';
            end case;
        end if;
    end process;

end Behavioral;
