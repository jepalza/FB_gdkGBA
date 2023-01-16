#Define _DMA_REP  (1 Shl  9)
#define _DMA_32   (1 Shl 10)
#define _DMA_IRQ  (1 Shl 14)
#define _DMA_ENB  (1 Shl 15)

Enum dma_timing_e 
    _IMMEDIATELY = 0,
    _VBLANK      = 1,
    _HBLANK      = 2,
    _SPECIAL     = 3
End Enum 

Dim Shared As ULong dma_src_addr(3) 
Dim Shared As ULong dma_dst_addr(3) 

Dim Shared As ULong dma_count(3) 

Declare Sub dma_transfer(timing As dma_timing_e) 
Declare Sub dma_transfer_fifo(ch As uByte) 
