

static Shared As uByte pscale_shift_lut(3) = { 0, 6, 8, 10 } 

Sub timers_clock(cycles As ULong) 
    Dim As UByte idx 
    Dim As BOOL overflow = FALSE 

    for  idx = 0 To 3        
        if (tmr(idx).ctrl.w And _TMR_ENB)=0 Then 
            overflow = FALSE 
            Continue For
        EndIf

        if (tmr(idx).ctrl.w And _TMR_CASCADE)<>0 Then 
            if (overflow) Then tmr(idx).count.w+=1  
        Else
            Dim As UByte shift = pscale_shift_lut(tmr(idx).ctrl.w And 3) 
            
            tmr_icnt(idx) += cycles
            Dim As ULong inc = tmr_icnt(idx) Shr shift 

            tmr(idx).count.w += inc 
            tmr_icnt(idx) -= inc Shl shift 
        EndIf
        
  		  overflow = IIf(tmr(idx).count.w>&hffff,1,0)
        if overflow Then 
            tmr(idx).count.w = tmr(idx).reload.w + (tmr(idx).count.w - &h10000) 
            if (((snd_pcm_vol.w Shr 10) And 1) = idx) Then 'DMA Sound A FIFO
                fifo_a_load() 
                if (fifo_a_len <= &h10) Then dma_transfer_fifo(1) 
            EndIf

            if (((snd_pcm_vol.w Shr 14) And 1) = idx) Then 'DMA Sound B FIFO
                fifo_b_load() 
                if (fifo_b_len <= &h10) Then dma_transfer_fifo(2) 
            EndIf
        EndIf

        if (tmr(idx).ctrl.w And _TMR_IRQ) And (overflow<>0) Then trigger_irq(TMR0_FLAG Shl idx)
    Next

End Sub
