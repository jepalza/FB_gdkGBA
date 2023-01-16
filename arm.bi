

#define ROR(valor2, st2)  (((valor2) Shr (st2)) Or ((valor2) Shl (32 - (st2))))
#define SBIT(operando)   IIf((operando And (1 Shl 20))<>0 , 1 , 0)

#define _ARM_BYTE_SZ   1
#define _ARM_HWORD_SZ  2
#define _ARM_WORD_SZ   4
#define _ARM_DWORD_SZ  8

'PSR flags
#define _ARM_N  (1 Shl 31) 'Negative
#define _ARM_Z  (1 Shl 30) 'Zero
#define _ARM_C  (1 Shl 29) 'Carry
#define _ARM_V  (1 Shl 28) 'Overflow
#define _ARM_Q  (1 Shl 27) 'Saturation
#define _ARM_A  (1 Shl  8) 'Abort off
#define _ARM_I  (1 Shl  7) 'IRQ off
#define _ARM_F  (1 Shl  6) 'FIQ off
#define _ARM_T  (1 Shl  5) 'Thumb

'Modes
#define _ARM_USR  &b10000 'User
#define _ARM_FIQ  &b10001 'Fast IRQ
#define _ARM_IRQ  &b10010 'IRQ
#define _ARM_SVC  &b10011 'Supervisor Call
#define _ARM_MON  &b10110 'Monitor
#define _ARM_ABT  &b10111 'Abort
#define _ARM_UND  &b11011 'Undefined
#define _ARM_SYS  &b11111 'System

'Interrupt addresses
#define _ARM_VEC_RESET   &h00 'Reset
#define _ARM_VEC_UND     &h04 'Undefined
#define _ARM_VEC_SVC     &h08 'Supervisor Call
#define _ARM_VEC_PABT    &h0c 'Prefetch Abort
#define _ARM_VEC_DABT    &h10 'Data Abort
#define _ARM_VEC_ADDR26  &h14 'Address exceeds 26 bits (legacy)
#define _ARM_VEC_IRQ     &h18 'IRQ
#define _ARM_VEC_FIQ     &h1c 'Fast IRQ

Union arm_word 
	As Long w
	'Type	
	'	As Short lo 
	'	As Short hi 
	'	Union
	'	  As Byte b0 
	'	  As Byte b1 
	'	  As Byte b2 
	'	  As Byte b3 
	'	End union
	'End Type
End Union 


'Parallel arithmetic
Type arm_parith_t 
    As arm_word lhs 
    As arm_word rhs 
    As UByte  rd 
End Type 


Type arm_regs_t 
    As ULong r(15) 

    As ULong r8_usr 
    As ULong r9_usr 
    As ULong r10_usr 
    As ULong r11_usr 
    As ULong r12_usr 
    As ULong r13_usr 
    As ULong r14_usr 

    As ULong r8_fiq 
    As ULong r9_fiq 
    As ULong r10_fiq 
    As ULong r11_fiq 
    As ULong r12_fiq 
    As ULong r13_fiq 
    As ULong r14_fiq 

    As ULong r13_irq 
    As ULong r14_irq 

    As ULong r13_svc 
    As ULong r14_svc 

    As ULong r13_mon 
    As ULong r14_mon 

    As ULong r13_abt 
    As ULong r14_abt 

    As ULong r13_und 
    As ULong r14_und 

    As ULong cpsr 

    As ULong spsr_fiq 
    As ULong spsr_irq 
    As ULong spsr_svc 
    As ULong spsr_abt 
    As ULong spsr_und 
    As ULong spsr_mon 
End Type 

Dim Shared As arm_regs_t arm_r 

Dim Shared As ULong arm_op 
Dim Shared As ULong arm_pipe(2) 
Dim Shared As ULong arm_cycles 

Dim Shared As BOOL int_halt 
Dim Shared As BOOL pipe_reload 

Declare Sub arm_init() 
Declare Sub arm_uninit() 

Declare Sub arm_exec(target_cycles As ULong) 

Declare Sub arm_int(ADDRESS As ULong , mode As Byte) 

Declare Sub arm_check_irq() 

Declare Sub arm_reset() 


/'
 * Execute
 '/

'Data Processing (Arithmetic)
Type arm_data_t 
    As ULongInt lhs 
    As ULongInt rhs 
    As UByte rd 
    As BOOL  cout 
    As BOOL  s 
End Type 


'Data Processing (Logical)
Enum arm_logic_e 
    ARD, ' AND
    BIC,
    EOR,
    MVN,
    ORN,
    ORR,
    SHF ' SHIFT
End Enum 


'Data Processing Operand Decoding
Type arm_shifter_t 
    As ULong valor 
    As BOOL  cout 
End Type 


'Multiplication
Type arm_mpy_t 
    As ULongInt lhs 
    As ULongInt rhs 
    As UByte ra 
    As UByte rd 
    As BOOL  s 
End Type 


'Processor State
#define PRIV_MASK   &hf8ff03df
#define USR_MASK    &hf8ff0000
#define STATE_MASK  &h01000020

Type arm_psr_t 
    As UByte rd 
    As ULong psr 
    As ULong mask 
    As BOOL  r 
End Type 

#define _ARM_MPY_SIGNED  0
#define _ARM_MPY_UNSIGN  1

#define _ARM_ARITH_SUB  0
#define _ARM_ARITH_ADD  1

#define _ARM_ARITH_NO_C   0
#define _ARM_ARITH_CARRY  1

#define _ARM_ARITH_NO_REV   0
#define _ARM_ARITH_REVERSE  1

#Define _ARM_FLAG_KEEP  0
#define _ARM_FLAG_SET   1

#define _ARM_SHIFT_LEFT   0
#define _ARM_SHIFT_RIGHT  1

#define _ARM_COND_UNCOND  &b1111

'Load/Store
Type arm_memio_t 
    As UShort regs 
    As UByte  rn 
    As UByte  rt2 
    As UByte  rt 
    As ULong  addr 
    As Long   disp 
End Type 

Enum arm_size_e 
     _BYTE  = 1,
     _HWORD = 2,
     _WORD  = 4,
     _DWORD = 8
End Enum 


Static Shared call_thumb_proc As Sub()
Static Shared call_arm_proc As Sub()

' max ROM GBA 32mb
Static Shared As LongInt max_rom_sz= 32 * 1024 * 1024 


' equivalente al "complemento a 1" del C (comando ~)
Function INV(aa As Long) As Long
	Return Not aa
End Function


Function to_pow2(valor As ULong) As ULong 
    valor-=1  

    valor Or= (valor Shr  1) 
    valor Or= (valor Shr  2) 
    valor Or= (valor Shr  4) 
    valor Or= (valor Shr  8) 
    valor Or= (valor Shr 16) 

    return valor + 1 
End Function