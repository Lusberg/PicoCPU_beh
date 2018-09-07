library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_misc.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;
use IEEE.NUMERIC_STD.ALL;

entity CPU is
    port    ( clk               : in std_logic;
              rst               : in std_logic;
              PC_output         : out std_logic_vector (7 downto 0);
              ACC_output        : out std_logic_vector (7 downto 0);
              Flag_output       : out std_logic_vector (3 downto 0)
);
end CPU;
architecture Behavioral of CPU is
---------------------------------------------
--      Component declarations
---------------------------------------------
component InstMem is
    generic ( BitWidth          : integer;
              InstructionWidth  : integer 
);
    port    ( address           : in std_logic_vector (BitWidth-1 downto 0);
              data              : out std_logic_vector (InstructionWidth-1 downto 0)
);
end component;

component Mem is
    generic ( BitWidth          : integer 
);  
    port    ( RdAddress         : in std_logic_vector (BitWidth-1 downto 0);
              Data_in           : in std_logic_vector (BitWidth-1 downto 0);
              WrtAddress        : in std_logic_vector (BitWidth-1 downto 0);
              clk               : in std_logic;
              RW                : in std_logic;
              rst               : in std_logic;
              Data_Out          : out std_logic_vector (BitWidth-1 downto 0) 
);
end component;
---------------------------------------------
--      Constants
---------------------------------------------
----2bits-----|----6bits-----|----8bits------|
----UNUSED----|----OPCODE----|----OPERAND----|
--------------|----------INSTWIDTH-----------|
constant CPU_BitWidth           : integer := 8;
constant CPU_InstWidth          : integer := 6 + CPU_Bitwidth;
---------------------------------------------
--      Functions
---------------------------------------------
function add_sub ( a1, b1 : std_logic_vector (CPU_BitWidth-1 downto 0); c_i, ctrl : std_logic) return std_logic_vector is
        variable a1_t, b1_t, sum_t : std_logic_vector (CPU_BitWidth+1 downto 0);
    begin
        a1_t := '0' & a1 & '1';
        if ctrl = '0' then
            b1_t := '0' & b1 & c_i;
        else
            b1_t := not(std_logic_vector('0' & b1)) & c_i;
        end if;
        sum_t := a1_t + b1_t;
        return sum_t (CPU_BitWidth+1 downto 1);
    end add_sub;
--add (a+b) is 'a', 'b', '0', '0'
--sub (a-b) is 'a', 'b', '1', '1'
---------------------------------------------
--      Types
---------------------------------------------
TYPE RegisterFile IS array (7 downto 0) of std_logic_vector (CPU_Bitwidth-1 downto 0);
TYPE Instruction IS ( PUSH,POP,
                      JMPEQ,Jmp_rel,Jmp,JmpZ,JmpOV,JmpC,
                      FlipA,And_A_R,OR_A_R,XOR_A_R,NegA,
                      ShiftA_R,ShiftA_L,ShiftArithL,ShiftArithR,
                      RRC,RLC,
                      LoadPC,SavePC,
                      Add_A_R, Add_A_Mem,Add_A_Dir, Sub_A_R,Sub_A_Mem,Sub_A_Dir,IncA,DecA,
                      Load_A_Mem,Load_R0_Mem,Load_R0_Dir,Store_A_Mem,load_A_R,load_R_A,Load_Ind_A,
                      ClearZ,ClearOV,ClearC, ClearACC,
                      NOP,HALT
);
---------------------------------------------
--      Signals
---------------------------------------------
signal RegFile : RegisterFile; --for TYPE RegisterFile
signal Instr : Instruction; --for TYPE Instruction
signal InstrReg, InstrReg_Out : std_logic_vector (CPU_InstWidth-1 downto 0);
signal InstrAdd, Mem_Rd_Address, Mem_Wrt_Address, DPUData, MEMDATA, DPU_Result: std_logic_vector (CPU_BitWidth-1 downto 0);
signal MemRW : std_logic;
signal PC : std_logic_vector (CPU_BitWidth-1 downto 0) := (others => '0'); --Program Counter
signal Next_PC : std_logic_vector (CPU_BitWidth-1 downto 0) := (others => '0'); --Next Program Counter
signal ACC_tmp : std_logic_vector (CPU_BitWidth downto 0) := (others => '0'); --Accumulator with C-bit @ pos (8)
signal OP : std_logic_vector (CPU_BitWidth-1 downto 0) := (others => '0'); --Operand
signal SP : std_logic_vector (CPU_BitWidth-1 downto 0) := (others => '0'); --Stack Pointer
signal who_cares : std_logic := '-';
---------------------------------------------
--      Flags
---------------------------------------------
signal C_flag : std_logic := '0'; --carry
signal EQ_flag : std_logic := '0'; --equal
signal Z_flag : std_logic := '0'; --zero
signal OV_flag : std_logic := '0'; --overflow
---------------------------------------------
--      Aliases
---------------------------------------------
alias opcode : std_logic_vector (5 downto 0) is InstrReg (CPU_instWidth-1 downto CPU_BitWidth);
alias operand : std_logic_vector (CPU_BitWidth-1 downto 0) is InstrReg (CPU_BitWidth-1 downto 0);
alias Reg : std_logic_vector (2 downto 0) is operand (2 downto 0);
alias ACC : std_logic_vector (CPU_BitWidth-1 downto 0) is ACC_tmp (CPU_BitWidth-1 downto 0); --Accumulator
alias C_bit : std_logic is ACC_tmp(CPU_BitWidth); --carry-bit in ACC_tmp @ pos (8) //doesn't work

begin
---------------------------------------------
--      Instruction memory
---------------------------------------------
InstMem_comp: InstMem 
generic map ( BitWidth => CPU_BitWidth, 
              InstructionWidth => CPU_InstWidth)
port map    ( address => PC, 
              data => InstrReg_Out
);
---------------------------------------------
--      Memory
---------------------------------------------
Mem_comp: Mem 
generic map ( BitWidth => CPU_BitWidth)
port map    ( RdAddress => Mem_Rd_Address, 
              Data_in => DPUData, 
              WrtAddress => Mem_Wrt_Address, 
              clk => clk, 
              RW => MemRW, 
              rst => rst, 
              Data_Out => DPU_Result
); 
---------------------------------------------
--      Output
---------------------------------------------
ACC_output <= ACC;
Flag_output <= C_flag & EQ_flag & Z_flag & OV_flag; --(3,2,1,0)


process
begin
---------------------------------------------
--      Flags
---------------------------------------------
C_flag <= '1'; --force to 1, then it works.

if ACC = OP then
    EQ_flag <= '1';
elsif ACC /= OP then
    EQ_flag <= '0';
end if;

if ACC = "00000000" then
    Z_flag <= '1';
elsif ACC /= "00000000" then
    Z_flag <= '0';
end if;

OV_flag <= (ACC(CPU_BitWidth-1) and Operand(CPU_BitWidth-1) and (not ACC(CPU_BitWidth-1))) or ((not ACC(CPU_BitWidth-1)) and (not Operand(CPU_BitWidth-1)) and ACC(CPU_BitWidth-1));
--OV_Flag_Value <= (ACC_in(BitWidth-1 ) and Mux_Out(BitWidth-1 ) and (not ACC_out(BitWidth-1 ))) or ((not ACC_in(BitWidth-1 )) and (not Mux_Out(BitWidth-1)) and ACC_out(BitWidth-1));
--if 
--end if
---------------------------------------------
--      Fetch
---------------------------------------------
wait until clk'event and clk = '1';
    if rst = '1' then
        PC <= (others => '0');
        ACC <= (others => '0');
        C_flag <= '0';
        EQ_flag <= '0';
        Z_flag <= '0';
        OV_flag <= '0';
    end if;
    InstrReg <= InstrReg_Out;
---------------------------------------------
--      Decode
---------------------------------------------
wait until clk'event and clk = '1';
    if rst = '1' then
        PC <= (others => '0');
        ACC <= (others => '0');
        C_flag <= '0';
        EQ_flag <= '0';
        Z_flag <= '0';
        OV_flag <= '0';
    end if;
---------------------------------------------
--      Instruction Decoder
---------------------------------------------
case opcode is
    when "000000"       => Instr <= Add_A_R;        OP <= RegFile (to_integer(unsigned(Reg)));
    when "000001"       => Instr <= Add_A_Mem;      OP <= (others => who_cares);
    when "000010"       => Instr <= Add_A_Dir;      OP <= operand;    
    when "000011"       => Instr <= Sub_A_R;        OP <= RegFile (to_integer(unsigned(Reg)));
    when "000100"       => Instr <= Sub_A_Mem;      OP <= (others => who_cares);
    when "000101"       => Instr <= Sub_A_Dir;      OP <= operand;
    when "000110"       => Instr <= IncA;           OP <= (others => who_cares);
    when "000111"       => Instr <= DecA;           OP <= (others => who_cares);  
    when "001000"       => Instr <= ShiftArithR;    OP <= (others => who_cares);
    when "001001"       => Instr <= ShiftArithL;    OP <= (others => who_cares); 
    when "001010"       => Instr <= ShiftA_R;       OP <= (others => who_cares);
    when "001011"       => Instr <= ShiftA_L;       OP <= (others => who_cares);
    when "001100"       => Instr <= RRC;            OP <= (others => who_cares);  
    when "001101"       => Instr <= RLC;            OP <= (others => who_cares);
    when "001110"       => Instr <= And_A_R;        OP <= RegFile (to_integer(unsigned(Reg)));
    when "001111"       => Instr <= OR_A_R;         OP <= RegFile (to_integer(unsigned(Reg)));
    when "010000"       => Instr <= XOR_A_R;        OP <= RegFile (to_integer(unsigned(Reg)));
    when "010001"       => Instr <= FlipA;          OP <= (others => who_cares);
    when "010010"       => Instr <= NegA;           OP <= (others => who_cares);
    when "010011"       => Instr <= Jmp;            OP <= (others => who_cares);
    when "010100"       => Instr <= JmpZ;           OP <= (others => who_cares);
    when "010101"       => Instr <= JmpOV;          OP <= (others => who_cares);
    when "010110"       => Instr <= JmpC;           OP <= (others => who_cares);
    when "010111"       => Instr <= Jmp_rel;        OP <= (others => who_cares);
    when "011000"       => Instr <= JMPEQ;          OP <= (others => who_cares);
    when "011001"       => Instr <= ClearZ;         OP <= (others => who_cares);
    when "011010"       => Instr <= ClearOV;        OP <= (others => who_cares);
    when "011011"       => Instr <= ClearC;         OP <= (others => who_cares);
    when "011100"       => Instr <= ClearACC;       OP <= (others => who_cares);  
    when "011101"       => Instr <= LoadPC;         OP <= (others => who_cares);
    when "011110"       => Instr <= SavePC;         OP <= (others => who_cares);     
    when "011111"       => Instr <= Load_A_Mem;     OP <= (others => who_cares);
    when "100000"       => Instr <= Store_A_Mem;    OP <= (others => who_cares);
    when "100001"       => Instr <= Load_R0_Dir;    OP <= (others => who_cares);
    when "100010"       => Instr <= Load_R0_Mem;    OP <= (others => who_cares);  
    when "100011"       => Instr <= load_A_R;       OP <= (others => who_cares);
    when "100100"       => Instr <= load_R_A;       OP <= (others => who_cares);
    when "100101"       => Instr <= Load_Ind_A ;    OP <= (others => who_cares);  
    when "111100"       => Instr <= PUSH;           OP <= (others => who_cares);
    when "111101"       => Instr <= POP;            OP <= (others => who_cares);
    when "111110"       => Instr <= NOP;            OP <= (others => who_cares);
    when "111111"       => Instr <= HALT;           OP <= (others => who_cares);
    when others         => Instr <= NOP;            OP <= (others => who_cares);
end case;
---------------------------------------------
--      Execute
---------------------------------------------
wait until clk'event and clk = '1';
    if rst = '1' then
        PC <= (others => '0');
        ACC <= (others => '0');
        C_flag <= '0';
        EQ_flag <= '0';
        Z_flag <= '0';
        OV_flag <= '0';
    end if;
---------------------------------------------
--      Instructions
---------------------------------------------
case Instr is
    when Add_A_R        => ACC_tmp <= add_sub (ACC, OP, '0', '0'); Next_PC <= PC+1;
    when Add_A_Mem      => Mem_Rd_Address <= operand; MemRW <= '0'; Next_PC <= PC+1;
    when Add_A_Dir      => ACC_tmp <= add_sub (ACC, OP, '0', '0'); Next_PC <= PC+1;
    when Sub_A_R        => ACC_tmp <= add_sub (ACC, OP, '1', '1'); Next_PC <= PC+1;
    when Sub_A_Mem      => Mem_Rd_Address <= operand; MemRW <= '0'; Next_PC <= PC+1;
    when Sub_A_Dir      => ACC_tmp <= add_sub (ACC, OP, '1', '1'); Next_PC <= PC+1;
    when IncA           => ACC_tmp <= add_sub (ACC, "00000001", '0', '0'); Next_PC <= PC+1;
    when DecA           => ACC_tmp <= add_sub (ACC, "00000001", '1', '1'); Next_PC <= PC+1;
    when ShiftArithR    => ACC <= ACC(7) & ACC(7 downto 1); Next_PC <= PC+1;
    when ShiftArithL    => ACC <= ACC(7) & ACC(5 downto 0) & '0'; Next_PC <= PC+1;
    when ShiftA_R       => ACC <= '0' & ACC(7 downto 1); Next_PC <= PC+1;
    when ShiftA_L       => ACC <= ACC(6 downto 0) & '0'; Next_PC <= PC+1;
    when RRC            => ACC <= C_flag & ACC(7 downto 1); C_flag <= ACC(0); Next_PC <= PC+1;
    when RLC            => ACC <= ACC(6 downto 0) & C_flag; C_flag <= ACC(7);  Next_PC <= PC+1;
    when And_A_R        => ACC <= ACC and OP; Next_PC <= PC+1;
    when OR_A_R         => ACC <= ACC or OP; Next_PC <= PC+1;
    when XOR_A_R        => ACC <= ACC xor OP; Next_PC <= PC+1;
    when FlipA          => ACC <= not ACC; Next_PC <= PC+1;
    when NegA           => ACC <= not ACC+1; Next_PC <= PC+1;
    when Jmp            => Next_PC <= operand;
    when JmpZ           => if Z_flag = '1' then Next_PC <= operand; else Next_PC <= PC+1; end if;
    when JmpOV          => if OV_flag = '1' then Next_PC <= operand; else Next_PC <= PC+1; end if;
    when JmpC           => if C_flag = '1' then Next_PC <= operand; else Next_PC <= PC+1; end if;
    when Jmp_rel        => PC <= Next_PC + operand;
    when JMPEQ          => if EQ_flag = '1' then Next_PC <= operand; else Next_PC <= PC+1; end if;
    when ClearZ         => Z_flag <= '0'; Next_PC <= PC+1;
    when ClearOV        => OV_flag <= '0'; Next_PC <= PC+1;
    when ClearC         => C_flag <= '0'; Next_PC <= PC+1;
    when ClearACC       => ACC <= (others => '0'); Next_PC <= PC+1;
    when LoadPC         => Next_PC <= ACC;
    when SavePC         => ACC <= PC; Next_PC <= PC+1;
    when Load_A_Mem     => Mem_Rd_Address <= operand; MemRW <= '0'; Next_PC <= PC+1;
    when Store_A_Mem    => DPUData <= ACC; Mem_Wrt_Address <= operand; MemRW <= '1'; Next_PC <= PC+1;
    when Load_R0_Dir    => RegFile(0) <= operand; Next_PC <= PC+1;
    when Load_R0_Mem    => Mem_Rd_Address <= operand; MemRW <= '0'; Next_PC <= PC+1;
    when load_A_R       => ACC <= RegFile(to_integer(unsigned(Reg))); Next_PC <= PC+1;
    when load_R_A       => for i in 0 to 7 loop if operand(i) = '1' then RegFile(i) <= ACC; end if; end loop; Next_PC <= PC+1;
    when Load_Ind_A     => Mem_Rd_Address <= ACC; MemRW <= '0'; Next_PC <= PC+1;
    when PUSH           => DPUData <= ACC; Mem_Wrt_Address <= SP; MemRW <= '1'; Next_PC <= PC+1;
    when POP            => Mem_Rd_Address <= std_logic_vector(unsigned(SP) - 1); MemRW <= '0'; Next_PC <= PC+1;
    when NOP            => Next_PC <= PC+1;
    when HALT           => Next_PC <= PC;
    when others         => Next_PC <= PC;
end case;
---------------------------------------------
--      WriteBack /mem
---------------------------------------------
--Separate because executed in 2nd clkstep
wait until clk'event and clk = '1';
    if rst = '1' then
        PC <= (others => '0');
        ACC <= (others => '0');
        C_flag <= '0';
        EQ_flag <= '0';
        Z_flag <= '0';
        OV_flag <= '0';
    end if;
---------------------------------------------
--      Mem Instructions
---------------------------------------------
case Instr is
    when Add_A_Mem      => ACC_tmp <= add_sub (ACC, DPU_Result, '0', '0');
    when Sub_A_Mem      => ACC_tmp <= add_sub (ACC, DPU_Result, '1', '1');
    when Load_A_Mem     => ACC <= DPU_Result;
    when Store_A_Mem    => MemRW <= '0';
    when Load_R0_Mem    => RegFile(0) <= DPU_Result;
    when Load_Ind_A     => ACC <= DPU_Result;
    when PUSH           => MemRW <= '0'; SP <= std_logic_vector(unsigned(SP) + 1);
    when POP            => ACC <= DPU_Result; SP <= std_logic_vector(unsigned(SP) - 1);
    when others         =>
end case;
PC <= Next_PC;
PC_output <= PC;

end process;
end Behavioral;