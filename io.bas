
Function io_read(ADDRESS As ULong) As uByte 
    io_open_bus = FALSE 

    Select Case As Const (ADDRESS)  
    	case &h04000000  
    		return disp_cnt.b0        And &hff 
    	case &h04000001  
    		return disp_cnt.b1        And &hff 
    	case &h04000002  
    		return green_inv.b0       And &h01 
    	case &h04000003  
    		return green_inv.b1       And &h00 
    	case &h04000004  
    		return disp_stat.b0       And &hff 
    	case &h04000005  
    		return disp_stat.b1       And &hff 
    	case &h04000006  
    		return v_count.b0         And &hff 
    	case &h04000007  
    		return v_count.b1         And &h00 
' -----------------------
    	case &h04000008  
    		return bg(0).ctrl.b0      And &hff 
    	case &h04000009  
    		return bg(0).ctrl.b1      And &hdf 
    	case &h0400000a  
    		return bg(1).ctrl.b0      And &hff 
    	case &h0400000b  
    		return bg(1).ctrl.b1      And &hdf 
    	case &h0400000c  
    		return bg(2).ctrl.b0      And &hff 
    	case &h0400000d  
    		return bg(2).ctrl.b1      And &hff 
    	case &h0400000e  
    		return bg(3).ctrl.b0      And &hff 
    	case &h0400000f  
    		return bg(3).ctrl.b1      And &hff 
' -----------------------
    	case &h04000048  
    		return win_in.b0          And &h3f 
    	case &h04000049  
    		return win_in.b1          And &h3f 
    	case &h0400004a  
    		return win_out.b0         And &h3f 
    	case &h0400004b  
    		return win_out.b1         And &h3f 
' -----------------------
    	case &h04000050  
    		return bld_cnt.b0         And &hff 
    	case &h04000051  
    		return bld_cnt.b1         And &h3f 
    	case &h04000052  
    		return bld_alpha.b0       And &h1f 
    	case &h04000053  
    		return bld_alpha.b1       And &h1f 
' -----------------------
    	case &h04000060  
    		return sqr_ch(0).sweep.b0 And &h7f 
    	case &h04000061  
    		return sqr_ch(0).sweep.b1 And &h00 
    	case &h04000062  
    		return sqr_ch(0).tone.b0  And &hc0 
    	case &h04000063  
    		return sqr_ch(0).tone.b1  And &hff 
    	case &h04000064  
    		return sqr_ch(0).ctrl.b0  And &h00 
    	case &h04000065  
    		return sqr_ch(0).ctrl.b1  And &h40 
    	case &h04000066  
    		return sqr_ch(0).ctrl.b2  And &h00 
    	case &h04000067  
    		return sqr_ch(0).ctrl.b3  And &h00 
' -----------------------
    	case &h04000068  
    		return sqr_ch(1).tone.b0  And &hc0 
    	case &h04000069  
    		return sqr_ch(1).tone.b1  And &hff 
    	case &h0400006c  
    		return sqr_ch(1).ctrl.b0  And &h00 
    	case &h0400006d  
    		return sqr_ch(1).ctrl.b1  And &h40 
    	case &h0400006e  
    		return sqr_ch(1).ctrl.b2  And &h00 
    	case &h0400006f  
    		return sqr_ch(1).ctrl.b3  And &h00 
' -----------------------
    	case &h04000070  
    		return wave_ch.wave.b0    And &he0 
    	case &h04000071  
    		return wave_ch.wave.b1    And &h00 
    	case &h04000072 
    		return wave_ch.volume.b0  And &h00 
    	case &h04000073  
    		return wave_ch.volume.b1  And &he0 
    	case &h04000074  
    		return wave_ch.ctrl.b0    And &h00 
    	case &h04000075  
    		return wave_ch.ctrl.b1    And &h40 
    	case &h04000076  
    		return wave_ch.ctrl.b2    And &h00 
    	case &h04000077  
    		return wave_ch.ctrl.b3    And &h00 
' -----------------------
    	case &h04000078  
    		return noise_ch.env.b0    And &h00 
    	case &h04000079  
    		return noise_ch.env.b1    And &hff 
    	case &h0400007a  
    		return noise_ch.env.b2    And &h00 
    	case &h0400007b  
    		return noise_ch.env.b3    And &h00 
    	case &h0400007c  
    		return noise_ch.ctrl.b0   And &hff 
    	case &h0400007d  
    		return noise_ch.ctrl.b1   And &h40 
    	case &h0400007e  
    		return noise_ch.ctrl.b2   And &h00 
    	case &h0400007f  
    		return noise_ch.ctrl.b3   And &h00 
' -----------------------
    	case &h04000080  
    		return snd_psg_vol.b0     And &h77 
    	case &h04000081  
    		return snd_psg_vol.b1     And &hff 
    	case &h04000082  
    		return snd_pcm_vol.b0     And &h0f 
    	case &h04000083  
    		return snd_pcm_vol.b1     And &h77 
    	case &h04000084  
    		return snd_psg_enb.b0     And &h8f 
    	case &h04000085  
    		return snd_psg_enb.b1     And &h00 
    	case &h04000086  
    		return snd_psg_enb.b2     And &h00 
    	case &h04000087  
    		return snd_psg_enb.b3     And &h00 
    	case &h04000088  
    		return snd_bias.b0        And &hff 
    	case &h04000089  
    		return snd_bias.b1        And &hc3 
    	case &h0400008a  
    		return snd_bias.b2        And &h00 
    	case &h0400008b  
    		return snd_bias.b3        And &h00 
' -----------------------
    	Case &h04000090 To &h0400009f  
            	Dim As UByte wave_bank = (wave_ch.wave.w Shr 2) And &h10 
            	Dim As UByte wave_idx  = (wave_bank Xor &h10) Or (ADDRESS And &hf) 
            	Return wave_ram(wave_idx) 
        
' -----------------------
    	case &h040000b8  
    		return dma_ch(0).count.b0 And &h00 
    	case &h040000b9  
    		return dma_ch(0).count.b1 And &h00 
    	case &h040000ba  
    		return dma_ch(0).ctrl.b0  And &he0 
    	case &h040000bb  
    		return dma_ch(0).ctrl.b1  And &hf7 
' -----------------------
    	case &h040000c4  
    		return dma_ch(1).count.b0 And &h00 
    	case &h040000c5  
    		return dma_ch(1).count.b1 And &h00 
    	case &h040000c6  
    		return dma_ch(1).ctrl.b0  And &he0 
    	case &h040000c7  
    		return dma_ch(1).ctrl.b1  And &hf7 
' -----------------------
    	case &h040000d0  
    		return dma_ch(2).count.b0 And &h00 
    	case &h040000d1  
    		return dma_ch(2).count.b1 And &h00 
    	case &h040000d2  
    		return dma_ch(2).ctrl.b0  And &he0 
    	case &h040000d3  
    		return dma_ch(2).ctrl.b1  And &hf7 
' -----------------------
    	case &h040000dc  
    		return dma_ch(3).count.b0 And &h00 
    	case &h040000dd  
    		return dma_ch(3).count.b1 And &h00 
    	case &h040000de  
    		return dma_ch(3).ctrl.b0  And &he0 
    	case &h040000df  
    		return dma_ch(3).ctrl.b1  And &hff 
' -----------------------
    	case &h04000100  
    		return tmr(0).count.b0    And &hff 
    	case &h04000101  
    		return tmr(0).count.b1    And &hff 
    	case &h04000102 
    		return tmr(0).ctrl.b0     And &hc7 
    	case &h04000103  
    		return tmr(0).ctrl.b1     And &h00 
' -----------------------
    	case &h04000104  
    		return tmr(1).count.b0    And &hff 
    	case &h04000105  
    		return tmr(1).count.b1    And &hff 
    	case &h04000106  
    		return tmr(1).ctrl.b0     And &hc7 
    	case &h04000107  
    		return tmr(1).ctrl.b1     And &h00 
' -----------------------
    	case &h04000108  
    		return tmr(2).count.b0    And &hff 
    	case &h04000109  
    		return tmr(2).count.b1    And &hff 
    	case &h0400010a  
    		return tmr(2).ctrl.b0     And &hc7 
    	case &h0400010b  
    		return tmr(2).ctrl.b1     And &h00 
' -----------------------
    	case &h0400010c  
    		return tmr(3).count.b0    And &hff 
    	case &h0400010d  
    		return tmr(3).count.b1    And &hff 
    	case &h0400010e  
    		return tmr(3).ctrl.b0     And &hc7 
    	case &h0400010f  
    		return tmr(3).ctrl.b1     And &h00 
' -----------------------
    	case &h04000120  
    		return sio_data32.b0      And &hff 
    	case &h04000121  
    		return sio_data32.b1      And &hff 
    	case &h04000122  
    		return sio_data32.b2      And &hff 
    	case &h04000123  
    		return sio_data32.b3      And &hff 
    	case &h04000128  
    		return sio_cnt.b0         And &hff 
    	case &h04000129  
    		return sio_cnt.b1         And &hff 
    	case &h0400012a  
    		return sio_data8.b0       And &hff 
    	case &h04000134  
    		return r_cnt.b0           And &hff 
    	case &h04000135  
    		return r_cnt.b1           And &hff 
' -----------------------
    	case &h04000130  
    		return key_input.b0       And &hff 
    	case &h04000131  
    		return key_input.b1       And &h3f 
' -----------------------
    	case &h04000200  
    		return int_enb.b0         And &hff 
    	case &h04000201  
    		return int_enb.b1         And &h3f 
    	case &h04000202  
    		return int_ack.b0         And &hff 
    	case &h04000203  
    		return int_ack.b1         And &h3f 
    	case &h04000204  
    		return wait_cnt.b0        And &hff 
    	case &h04000205  
    		return wait_cnt.b1        And &hdf 
    	case &h04000206  
    		return wait_cnt.b2        And &h00 
    	case &h04000207  
    		return wait_cnt.b3        And &h00 
    	case &h04000208  
    		return int_enb_m.b0       And &h01 
    	case &h04000209  
    		return int_enb_m.b1       And &h00 
    	case &h0400020a  
    		return int_enb_m.b2       And &h00 
    	case &h0400020b  
    		return int_enb_m.b3       And &h00 
' -----------------------
    	case &h04000300  
    		return post_boot          And &h01 
    	case &h04000301  
    		return int_halt           And &h00 
    
    End Select
    
    io_open_bus = TRUE 

    return 0 
End Function

Sub dma_load(ch As uByte , value As uByte) 
    Dim As UByte old = dma_ch(ch).ctrl.b1 

    dma_ch(ch).ctrl.b1 = value 

    if ((old Xor value) And value And &h80)<>0 Then 
        dma_dst_addr(ch) = dma_ch(ch).dst.w 
        dma_src_addr(ch) = dma_ch(ch).src.w 

        if (dma_ch(ch).ctrl.w And _DMA_32)<>0 Then 
            dma_dst_addr(ch) And= INV(3) 
            dma_src_addr(ch) And= INV(3) 
        Else
            dma_dst_addr(ch) And= INV(1) 
            dma_src_addr(ch) And= INV(1) 
        EndIf
        
        dma_count(ch) = dma_ch(ch).count.w 
        dma_transfer(_IMMEDIATELY) 
    EndIf
  
End Sub

Sub tmr_load(idx As uByte , value As uByte) 
    Dim As UByte old = tmr(idx).ctrl.b0 

    tmr(idx).ctrl.b0 = value 

    if (value And _TMR_ENB)<>0 Then 
        tmr_enb Or=  (1 Shl idx) 
    Else
        tmr_enb And=  INV(1 Shl idx)
    EndIf

    if ((old Xor value) And value And _TMR_ENB)<>0 Then 
        tmr(idx).count.w = tmr(idx).reload.w 
        tmr_icnt(idx) = 0 
    EndIf
  
End Sub

Sub snd_reset_state(ch As uByte , enb As BOOL) 
    if (enb) Then 
        snd_ch_state(ch).phase       = FALSE 
        snd_ch_state(ch).samples     = 0 
        snd_ch_state(ch).length_time = 0 
        snd_ch_state(ch).sweep_time  = 0 
        snd_ch_state(ch).env_time    = 0 

        if (ch = 2) Then wave_reset() 
        if (ch = 3) Then 
            if (noise_ch.ctrl.w And NOISE_7)<>0 Then 
                snd_ch_state(ch).lfsr = &h007f 
            Else
                snd_ch_state(ch).lfsr = &h7fff
            EndIf
        EndIf
  
        snd_psg_enb.w Or=(1 Shl ch) 
    EndIf
  
End Sub

Sub io_write(ADDRESS As ULong , value As uByte) 
    Select Case As Const (ADDRESS)  
    	Case &h04000000 
            if (arm_r.r(15) >= &h4000) Then 
                'The CGB mode enable bit 3 can only be set by the bios
                value And= &hf7 
            EndIf
            disp_cnt.b0 = value 
' -----------------------
    	case &h04000001  
    		disp_cnt.b1        =  value 
    	case &h04000002  
    		green_inv.b0       =  value 
    	case &h04000003  
    		green_inv.b1       =  value 
    	Case &h04000004 
            disp_stat.b0 And= &h47 
            disp_stat.b0 Or= value And INV(&h47 )
    	case &h04000005  
    		disp_stat.b1       =  value 
' -----------------------
    	case &h04000008  
    		bg(0).ctrl.b0      =  value 
    	case &h04000009  
    		bg(0).ctrl.b1      =  value 
    	case &h0400000a  
    		bg(1).ctrl.b0      =  value 
    	case &h0400000b  
    		bg(1).ctrl.b1      =  value 
    	case &h0400000c  
    		bg(2).ctrl.b0      =  value 
    	case &h0400000d  
    		bg(2).ctrl.b1      =  value 
    	case &h0400000e  
    		bg(3).ctrl.b0      =  value 
    	case &h0400000f  
    		bg(3).ctrl.b1      =  value 
' -----------------------
    	case &h04000010  
    		bg(0).xofs.b0      =  value 
    	case &h04000011  
    		bg(0).xofs.b1      =  value 
    	case &h04000012 
    		bg(0).yofs.b0      =  value 
    	case &h04000013  
    		bg(0).yofs.b1      =  value 
    	case &h04000014  
    		bg(1).xofs.b0      =  value 
    	case &h04000015  
    		bg(1).xofs.b1      =  value 
    	case &h04000016  
    		bg(1).yofs.b0      =  value 
    	case &h04000017  
    		bg(1).yofs.b1      =  value 
    	case &h04000018  
    		bg(2).xofs.b0      =  value 
    	case &h04000019  
    		bg(2).xofs.b1      =  value 
    	case &h0400001a  
    		bg(2).yofs.b0      =  value 
    	case &h0400001b  
    		bg(2).yofs.b1      =  value 
    	case &h0400001c  
    		bg(3).xofs.b0      =  value 
    	case &h0400001d  
    		bg(3).xofs.b1      =  value 
    	case &h0400001e  
    		bg(3).yofs.b0      =  value 
    	case &h0400001f  
    		bg(3).yofs.b1      =  value 
' -----------------------
    	case &h04000020  
    		bg_pa(2).b0        =  value 
    	case &h04000021 
    		bg_pa(2).b1        =  value 
    	case &h04000022  
    		bg_pb(2).b0        =  value 
    	case &h04000023  
    		bg_pb(2).b1        =  value 
    	case &h04000024  
    		bg_pc(2).b0        =  value 
    	case &h04000025  
    		bg_pc(2).b1        =  value 
    	case &h04000026  
    		bg_pd(2).b0        =  value 
    	case &h04000027  
    		bg_pd(2).b1        =  value 
' -----------------------
    	case &h04000028 
            bg_refxe(2).b0 = value
            bg_refxi(2).b0 = value 
    	case &h04000029 
            bg_refxe(2).b1 = value
            bg_refxi(2).b1 = value 
    	case &h0400002a 
            bg_refxe(2).b2 = value
            bg_refxi(2).b2 = value 
    	case &h0400002b 
            bg_refxe(2).b3 = value
            bg_refxi(2).b3 = value 
    	case &h0400002c 
            bg_refye(2).b0 = value
            bg_refyi(2).b0 = value 
    	case &h0400002d 
            bg_refye(2).b1 = value
            bg_refyi(2).b1 = value 
    	case &h0400002e 
            bg_refye(2).b2 = value
            bg_refyi(2).b2 = value 
    	case &h0400002f 
            bg_refye(2).b3 = value
            bg_refyi(2).b3 = value 
' -----------------------
    	case &h04000030  
    		bg_pa(3).b0 =  value 
    	case &h04000031  
    		bg_pa(3).b1 =  value 
    	case &h04000032  
    		bg_pb(3).b0 =  value 
    	case &h04000033  
    		bg_pb(3).b1 =  value 
    	case &h04000034  
    		bg_pc(3).b0 =  value 
    	case &h04000035  
    		bg_pc(3).b1 =  value 
    	case &h04000036  
    		bg_pd(3).b0 =  value 
    	case &h04000037  
    		bg_pd(3).b1 =  value 
' -----------------------
    	case &h04000038 
            bg_refxe(3).b0 = value
            bg_refxi(3).b0 = value 
    	case &h04000039 
            bg_refxe(3).b1 = value
            bg_refxi(3).b1 = value 
    	case &h0400003a 
            bg_refxe(3).b2 = value
            bg_refxi(3).b2 = value 
    	case &h0400003b 
            bg_refxe(3).b3 = value
            bg_refxi(3).b3 = value 
    	case &h0400003c 
            bg_refye(3).b0 = value
            bg_refyi(3).b0 = value 
    	case &h0400003d 
            bg_refye(3).b1 = value
            bg_refyi(3).b1 = value  
    	case &h0400003e 
            bg_refye(3).b2 = value
            bg_refyi(3).b2 = value 
    	case &h0400003f 
            bg_refye(3).b3 = value
            bg_refyi(3).b3 = value 
' -----------------------
    	case &h04000048  
    		win_in.b0 =  value 
    	case &h04000049  
    		win_in.b1 =  value 
    	case &h0400004a  
    		win_out.b0 =  value 
    	case &h0400004b  
    		win_out.b1 =  value 
' -----------------------
    	case &h04000050  
    		bld_cnt.b0 =  value 
    	case &h04000051  
    		bld_cnt.b1 =  value 
    	case &h04000052  
    		bld_alpha.b0 =  value 
    	case &h04000053  
    		bld_alpha.b1 =  value 
    	case &h04000054  
    		bld_bright.b0 =  value 
    	case &h04000055 
    		bld_bright.b1 =  value 
    	case &h04000056 
    		bld_bright.b2 =  value 
    	case &h04000057  
    		bld_bright.b3 =  value 
' -----------------------
    	case &h04000060 
            if (snd_psg_enb.w And PSG_ENB) Then 
                sqr_ch(0).sweep.b0 = value
            EndIf
    	case &h04000061 
            if (snd_psg_enb.w And PSG_ENB) Then 
                sqr_ch(0).sweep.b1 = value
            EndIf
    	case &h04000062 
            if (snd_psg_enb.w And PSG_ENB) Then 
                sqr_ch(0).tone.b0 = value
            EndIf
    	case &h04000063 
            if (snd_psg_enb.w And PSG_ENB) Then 
                sqr_ch(0).tone.b1 = value
            EndIf
    	case &h04000064 
            if (snd_psg_enb.w And PSG_ENB) Then 
                sqr_ch(0).ctrl.b0 = value
            EndIf
    	case &h04000065 
            if (snd_psg_enb.w And PSG_ENB) Then 
                sqr_ch(0).ctrl.b1 = value 
                snd_reset_state(0, value And &h80) 
            EndIf
    	case &h04000066 
            if (snd_psg_enb.w And PSG_ENB) Then 
                sqr_ch(0).ctrl.b2 = value
            EndIf
    	case &h04000067 
            if (snd_psg_enb.w And PSG_ENB) Then 
                sqr_ch(0).ctrl.b3 = value
            EndIf
    	case &h04000068 
            if (snd_psg_enb.w And PSG_ENB) Then 
                sqr_ch(1).tone.b0 = value
            EndIf
    	case &h04000069 
            if (snd_psg_enb.w And PSG_ENB) Then 
                sqr_ch(1).tone.b1 = value
            EndIf
    	case &h0400006c 
            if (snd_psg_enb.w And PSG_ENB) Then 
                sqr_ch(1).ctrl.b0 = value
            EndIf
    	case &h0400006d 
            if (snd_psg_enb.w And PSG_ENB) Then 
                sqr_ch(1).ctrl.b1 = value 
                snd_reset_state(1, value And &h80) 
            EndIf
    	case &h0400006e 
            if (snd_psg_enb.w And PSG_ENB) Then 
                sqr_ch(1).ctrl.b2 = value
            EndIf
    	case &h0400006f 
            if (snd_psg_enb.w And PSG_ENB) Then 
                sqr_ch(1).ctrl.b3 = value
            EndIf
    	case &h04000070 
            if (snd_psg_enb.w And PSG_ENB) Then 
                wave_ch.wave.b0 = value
            EndIf
    	case &h04000071 
            if (snd_psg_enb.w And PSG_ENB) Then 
                wave_ch.wave.b1 = value
            EndIf
    	case &h04000072 
            if (snd_psg_enb.w And PSG_ENB) Then 
                wave_ch.volume.b0 = value
            EndIf
    	case &h04000073 
            if (snd_psg_enb.w And PSG_ENB) Then 
                wave_ch.volume.b1 = value
            EndIf
    	case &h04000074 
            if (snd_psg_enb.w And PSG_ENB) Then 
                wave_ch.ctrl.b0 = value
            EndIf
    	case &h04000075 
            if (snd_psg_enb.w And PSG_ENB) Then 
                wave_ch.ctrl.b1 = value 
                snd_reset_state(2, value And &h80) 
            EndIf
    	case &h04000076 
            if (snd_psg_enb.w And PSG_ENB) Then 
                wave_ch.ctrl.b2 = value
            EndIf
    	case &h04000077 
            if (snd_psg_enb.w And PSG_ENB) Then 
                wave_ch.ctrl.b3 = value
            EndIf
    	case &h04000078 
            if (snd_psg_enb.w And PSG_ENB) Then 
                noise_ch.env.b0 = value
            EndIf
    	case &h04000079 
            if (snd_psg_enb.w And PSG_ENB) Then 
                noise_ch.env.b1 = value
            EndIf
    	case &h0400007a 
            if (snd_psg_enb.w And PSG_ENB) Then 
                noise_ch.env.b2 = value
            EndIf
    	case &h0400007b 
            if (snd_psg_enb.w And PSG_ENB) Then 
                noise_ch.env.b3 = value
            EndIf
    	case &h0400007c 
            if (snd_psg_enb.w And PSG_ENB) Then 
                noise_ch.ctrl.b0 = value
            EndIf
    	case &h0400007d 
            if (snd_psg_enb.w And PSG_ENB) Then 
                noise_ch.ctrl.b1 = value 
                snd_reset_state(3, value And &h80) 
            EndIf
    	case &h0400007e 
            if (snd_psg_enb.w And PSG_ENB) Then 
                noise_ch.ctrl.b2 = value
            EndIf
    	case &h0400007f 
            if (snd_psg_enb.w And PSG_ENB) Then 
                noise_ch.ctrl.b3 = value
            EndIf
    	case &h04000080 
            if (snd_psg_enb.w And PSG_ENB) Then 
                snd_psg_vol.b0 = value
            EndIf
    	case &h04000081 
            if (snd_psg_enb.w And PSG_ENB) Then 
                snd_psg_vol.b1 = value
            EndIf
    	case &h04000082 
            'PCM is not affected by the PSG Enable flag
            snd_pcm_vol.b0 = value 
    	case &h04000083 
            snd_pcm_vol.b1 = value 
            if (value And &h08) Then fifo_a_len = 0 
            if (value And &h80) Then fifo_b_len = 0 
    	case &h04000084 
            snd_psg_enb.b0 And= &hf 
            snd_psg_enb.b0 Or= value And INV(&hf) 

            if (value And PSG_ENB)=0 Then 
  
                sqr_ch(0).sweep.w = 0 
                sqr_ch(0).tone.w  = 0 
                sqr_ch(0).ctrl.w  = 0 

                sqr_ch(1).tone.w  = 0 
                sqr_ch(1).ctrl.w  = 0 

                wave_ch.wave.w    = 0 
                wave_ch.volume.w  = 0 
                wave_ch.ctrl.w    = 0 

                noise_ch.env.w    = 0 
                noise_ch.ctrl.w   = 0 

                snd_psg_vol.w     = 0 
                snd_psg_enb.w     = 0 
            
            EndIf
    	case &h04000085  
    		snd_psg_enb.b1     =  value 
    	case &h04000086  
    		snd_psg_enb.b2     =  value 
    	case &h04000087  
    		snd_psg_enb.b3     =  value 
    	case &h04000088  
    		snd_bias.b0        =  value 
    	case &h04000089  
    		snd_bias.b1        =  value 
    	case &h0400008a  
    		snd_bias.b2        =  value 
    	case &h0400008b  
    		snd_bias.b3        =  value 
' -----------------------
    	Case &h04000090 To &h0400009f
            Dim As UByte wave_bank = (wave_ch.wave.w Shr 2) And &h10 
            Dim As UByte wave_idx  = (wave_bank Xor &h10) Or (ADDRESS And &hf) 
            wave_ram(wave_idx) = value 
' -----------------------
    	case &h040000a0  
    		snd_fifo_a_0         =  value 
    	case &h040000a1  
    		snd_fifo_a_1         =  value 
    	case &h040000a2  
    		snd_fifo_a_2         =  value 
    	case &h040000a3  
    		snd_fifo_a_3         =  value 
' -----------------------
    	case &h040000a4  
    		snd_fifo_b_0         =  value 
    	case &h040000a5  
    		snd_fifo_b_1         =  value 
    	case &h040000a6  
    		snd_fifo_b_2         =  value 
    	case &h040000a7  
    		snd_fifo_b_3         =  value 
' -----------------------
    	case &h040000b0  
    		dma_ch(0).src.b0   =  value 
    	case &h040000b1  
    		dma_ch(0).src.b1   =  value 
    	case &h040000b2  
    		dma_ch(0).src.b2   =  value 
    	case &h040000b3  
    		dma_ch(0).src.b3   =  value 
    	case &h040000b4  
    		dma_ch(0).dst.b0   =  value 
    	case &h040000b5  
    		dma_ch(0).dst.b1   =  value 
    	case &h040000b6  
    		dma_ch(0).dst.b2   =  value 
    	case &h040000b7  
    		dma_ch(0).dst.b3   =  value 
    	case &h040000b8  
    		dma_ch(0).count.b0 =  value 
    	case &h040000b9  
    		dma_ch(0).count.b1 =  value 
    	case &h040000ba  
    		dma_ch(0).ctrl.b0  =  value 
    	case &h040000bb  
    		dma_load(0, value) 
' -----------------------
    	case &h040000bc  
    		dma_ch(1).src.b0   =  value 
    	case &h040000bd  
    		dma_ch(1).src.b1   =  value 
    	case &h040000be  
    		dma_ch(1).src.b2   =  value 
    	case &h040000bf  
    		dma_ch(1).src.b3   =  value 
    	case &h040000c0  
    		dma_ch(1).dst.b0   =  value 
    	case &h040000c1  
    		dma_ch(1).dst.b1   =  value 
    	case &h040000c2  
    		dma_ch(1).dst.b2   =  value 
    	case &h040000c3  
    		dma_ch(1).dst.b3   =  value 
    	case &h040000c4  
    		dma_ch(1).count.b0 =  value 
    	case &h040000c5  
    		dma_ch(1).count.b1 =  value 
    	case &h040000c6  
    		dma_ch(1).ctrl.b0  =  value 
    	case &h040000c7  
    		dma_load(1, value)
' -----------------------
    	case &h040000c8  
    		dma_ch(2).src.b0   =  value 
    	case &h040000c9  
    		dma_ch(2).src.b1   =  value 
    	case &h040000ca  
    		dma_ch(2).src.b2   =  value 
    	case &h040000cb  
    		dma_ch(2).src.b3   =  value 
    	case &h040000cc  
    		dma_ch(2).dst.b0   =  value 
    	case &h040000cd 
    		dma_ch(2).dst.b1   =  value 
    	case &h040000ce  
    		dma_ch(2).dst.b2   =  value 
    	case &h040000cf  
    		dma_ch(2).dst.b3   =  value 
    	case &h040000d0  
    		dma_ch(2).count.b0 =  value 
    	case &h040000d1  
    		dma_ch(2).count.b1 =  value 
    	case &h040000d2  
    		dma_ch(2).ctrl.b0  =  value 
    	case &h040000d3  
    		dma_load(2, value)
' -----------------------
    	case &h040000d4  
    		dma_ch(3).src.b0   =  value 
    	case &h040000d5  
    		dma_ch(3).src.b1   =  value 
    	case &h040000d6  
    		dma_ch(3).src.b2   =  value 
    	case &h040000d7  
    		dma_ch(3).src.b3   =  value 
    	case &h040000d8  
    		dma_ch(3).dst.b0   =  value 
    	case &h040000d9  
    		dma_ch(3).dst.b1   =  value 
    	case &h040000da  
    		dma_ch(3).dst.b2   =  value 
    	case &h040000db  
    		dma_ch(3).dst.b3   =  value 
    	case &h040000dc  
    		dma_ch(3).count.b0 =  value 
    	case &h040000dd  
    		dma_ch(3).count.b1 =  value 
    	case &h040000de 
    		dma_ch(3).ctrl.b0  =  value 
    	case &h040000df  
    		dma_load(3, value)
' -----------------------
    	case &h04000100  
    		tmr(0).reload.b0   =  value 
    	case &h04000101  
    		tmr(0).reload.b1   =  value 
    	case &h04000102  
    		tmr_load(0, value)
    	case &h04000103  
    		tmr(0).ctrl.b1     =  value 
' -----------------------
    	case &h04000104  
    		tmr(1).reload.b0   =  value 
    	case &h04000105  
    		tmr(1).reload.b1   =  value 
    	case &h04000106  
    		tmr_load(1, value)
    	case &h04000107  
    		tmr(1).ctrl.b1     =  value 
' -----------------------
    	case &h04000108  
    		tmr(2).reload.b0   =  value 
    	case &h04000109  
    		tmr(2).reload.b1   =  value 
    	case &h0400010a  
    		tmr_load(2, value)
    	case &h0400010b  
    		tmr(2).ctrl.b1     =  value 
' -----------------------
    	case &h0400010c  
    		tmr(3).reload.b0   =  value 
    	case &h0400010d  
    		tmr(3).reload.b1   =  value 
    	case &h0400010e  
    		tmr_load(3, value)
    	case &h0400010f  
    		tmr(3).ctrl.b1     =  value 
' -----------------------
    	case &h04000120  
    		sio_data32.b0      =  value 
    	case &h04000121  
    		sio_data32.b1      =  value 
    	case &h04000122  
    		sio_data32.b2      =  value 
    	case &h04000123  
    		sio_data32.b3      =  value 
    	case &h04000128  
    		sio_cnt.b0         =  value 
    	case &h04000129  
    		sio_cnt.b1         =  value 
    	case &h0400012a  
    		sio_data8.b0       =  value 
    	case &h04000134  
    		r_cnt.b0           =  value 
    	case &h04000135  
    		r_cnt.b1           =  value 
' -----------------------
    	case &h04000200 
            int_enb.b0 = value 
            arm_check_irq()  
    	case &h04000201 
            int_enb.b1 = value 
            arm_check_irq() 
    	case &h04000202  
    		int_ack.b0 And= INV(value) 
    	case &h04000203  
    		int_ack.b1 And= INV(value)
    	case &h04000204 
            wait_cnt.b0 = value 
            update_ws() 
    	case &h04000205 
            wait_cnt.b1 = value 
            update_ws() 
    	case &h04000206 
            wait_cnt.b2 = value 
            update_ws() 
    	case &h04000207 
            wait_cnt.b3 = value 
            update_ws() 
    	case &h04000208 
            int_enb_m.b0 = value 
            arm_check_irq() 
    	case &h04000209 
            int_enb_m.b1 = value 
            arm_check_irq() 
    	case &h0400020a 
            int_enb_m.b2 = value 
            arm_check_irq() 
    	case &h0400020b 
            int_enb_m.b3 = value 
            arm_check_irq() 
' -----------------------
    	case &h04000300  
    		post_boot            =  value 
    	Case &h04000301  
    		int_halt             =  TRUE 
    
   End Select

End Sub

Sub trigger_irq(flag As uShort) 
    int_ack.w Or= flag 
    int_halt = FALSE 
    arm_check_irq() 
End Sub

static Shared As uByte ws0_s_lut(1) = { 2, 1 } 
Static Shared As uByte ws1_s_lut(1) = { 4, 1 } 
Static Shared As uByte ws2_s_lut(1) = { 8, 1 } 
Static Shared As uByte ws_n_lut(3)  = { 4, 3, 2, 8 } 

Sub update_ws() 
    ws_n(0) = ws_n_lut((wait_cnt.w Shr 2) And 3) 
    ws_n(1) = ws_n_lut((wait_cnt.w Shr 5) And 3) 
    ws_n(2) = ws_n_lut((wait_cnt.w Shr 8) And 3) 

    ws_s(0) = ws0_s_lut((wait_cnt.w Shr  4) And 1) 
    ws_s(1) = ws1_s_lut((wait_cnt.w Shr  7) And 1) 
    ws_s(2) = ws2_s_lut((wait_cnt.w Shr 10) And 1) 

    Dim As Byte i 

    for i = 0 To 2         
        ws_n_t16(i) = ws_n(i) + 1 
        ws_s_t16(i) = ws_s(i) + 1 

        ws_n_arm(i) = ws_n_t16(i) + ws_s_t16(i) 
        ws_s_arm(i) = ws_s_t16(i) Shl 1 
    Next

End Sub
