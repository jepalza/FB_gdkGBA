

'TODO: Timing - DMA should take some amount of cycles

Sub dma_transfer(timing As dma_timing_e) 
    Dim As UByte ch 

    for ch = 0 To 3         
        If ((dma_ch(ch).ctrl.w And _DMA_ENB)=0) Or (((dma_ch(ch).ctrl.w Shr 12) And 3) <> timing) Then Continue For

        if (ch = 3) Then eeprom_idx = 0

        Dim As Byte unit_size = IIf(dma_ch(ch).ctrl.w And _DMA_32 , 4 , 2) 

        Dim As BOOL dst_reload = FALSE 

        Dim As Byte dst_inc = 0 
        Dim As Byte src_inc = 0 

        Select Case As Const ((dma_ch(ch).ctrl.w Shr 5) And 3)  
        	case 0  
        		dst_inc =  unit_size
        	case 1  
        		dst_inc = -unit_size 
        	Case 3 
            dst_inc =  unit_size 
            dst_reload = TRUE 
        End Select


        Select Case As Const ((dma_ch(ch).ctrl.w Shr 7) And 3)  
        	case 0  
        		src_inc =  unit_size
        	case 1  
        		src_inc = -unit_size 
        End Select


        while dma_count(ch) 
        	   dma_count(ch)-=1
            if (dma_ch(ch).ctrl.w And _DMA_32) Then 
                arm_write(dma_dst_addr(ch),  arm_read(dma_src_addr(ch))) 
            Else
                arm_writeh(dma_dst_addr(ch), arm_readh(dma_src_addr(ch)))
            EndIf

            dma_dst_addr(ch) += dst_inc 
            dma_src_addr(ch) += src_inc 
        Wend

        if (dma_ch(ch).ctrl.w And _DMA_IRQ) Then 
            trigger_irq(DMA0_FLAG Shl ch)
        EndIf

        if (dma_ch(ch).ctrl.w And _DMA_REP) Then 
            dma_count(ch) = dma_ch(ch).count.w 
            if (dst_reload) Then dma_dst_addr(ch) = dma_ch(ch).dst.w 
            Continue For 
        EndIf
  
        dma_ch(ch).ctrl.w And=  INV(_DMA_ENB) 
    
    Next

End Sub

Sub dma_transfer_fifo(ch As uByte) 
    if ( (dma_ch(ch).ctrl.w And _DMA_ENB)=0) Or (((dma_ch(ch).ctrl.w Shr 12) And 3) <> _SPECIAL) Then 
        Exit Sub
    EndIf

    Dim As UByte i 

    for i = 0 To 3         
        arm_write(dma_dst_addr(ch), arm_read(dma_src_addr(ch))) 
        if (ch = 1) Then 
            fifo_a_copy() 
        Else
            fifo_b_copy()
        EndIf

        Select Case As Const ((dma_ch(ch).ctrl.w Shr 7) And 3)  
        	case 0  
        		dma_src_addr(ch) += 4
        	case 1  
        		dma_src_addr(ch) -= 4
        End Select
    Next

    if (dma_ch(ch).ctrl.w And _DMA_IRQ) Then 
        trigger_irq(DMA0_FLAG Shl ch)
    EndIf
    
End Sub
