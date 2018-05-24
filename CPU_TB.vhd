library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_misc.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;
use IEEE.NUMERIC_STD.ALL;

entity CPU_TB is

end CPU_TB;

architecture Behavioral of CPU_TB is

component CPU is
    port    ( clk               : in std_logic;
              rst               : in std_logic;
              PC_output         : out std_logic_vector (7 downto 0);
              ACC_output        : out std_logic_vector (7 downto 0);
              Flag_output       : out std_logic_vector (3 downto 0)
);
end component;

constant clk_period             : time      := 10 ns;
signal clk                      : std_logic := '0';
signal rst                      : std_logic;
signal PC_output                : std_logic_vector (7 downto 0);
signal ACC_output               : std_logic_vector (7 downto 0);
signal Flag_output              : std_logic_vector (3 downto 0);

begin

UUT : CPU
    port map ( clk => clk,
               rst => rst,
               PC_output => PC_output,
               ACC_output => ACC_output,
               Flag_output => Flag_output
);

clock_process : process
    begin
        clk <= '0';
        wait for clk_period/2;
        clk <= '1';
        wait for clk_period/2;
end process;

stimuli : process
    begin
        rst <= '1'; wait for clk_period/2; rst <= '0'; wait for clk_period*3;
        wait;

end process;
end Behavioral;