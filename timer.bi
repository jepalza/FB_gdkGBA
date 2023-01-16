#define _TMR_CASCADE  (1 Shl 2)
#define _TMR_IRQ      (1 Shl 6)
#define _TMR_ENB      (1 Shl 7)

Dim Shared As ULong tmr_icnt(4) 

Dim Shared As uByte tmr_enb 
Dim Shared As uByte tmr_irq 
Dim Shared As uByte tmr_ie 

Declare Sub timers_clock(cycles As ULong) 
