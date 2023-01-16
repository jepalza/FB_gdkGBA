

Union io_reg 
   As ULong w 
	Type
	    As UByte b0 
	    As UByte b1 
	    As UByte b2 
	    As UByte b3 
	End Type 
End union 

#define VBLK_FLAG  (1 Shl  0)
#define HBLK_FLAG  (1 Shl  1)
#define VCNT_FLAG  (1 Shl  2)
#define TMR0_FLAG  (1 Shl  3)
#define TMR1_FLAG  (1 Shl  4)
#define TMR2_FLAG  (1 Shl  5)
#define TMR3_FLAG  (1 Shl  6)
#define DMA0_FLAG  (1 Shl  8)
#define DMA1_FLAG  (1 Shl  9)
#define DMA2_FLAG  (1 Shl 10)
#define DMA3_FLAG  (1 Shl 11)

#define MAP_1D_FLAG  (1 Shl  6)
#define BG0_ENB      (1 Shl  8)
#define BG1_ENB      (1 Shl  9)
#define BG2_ENB      (1 Shl 10)
#define BG3_ENB      (1 Shl 11)
#define OBJ_ENB      (1 Shl 12)

#define VBLK_IRQ  (1 Shl  3)
#define HBLK_IRQ  (1 Shl  4)
#define VCNT_IRQ  (1 Shl  5)

Dim Shared As io_reg disp_cnt 
Dim Shared As io_reg green_inv 
Dim Shared As io_reg disp_stat 
Dim Shared As io_reg v_count 

Type bg_t 
    As io_reg ctrl 
    As io_reg xofs 
    As io_reg yofs 
End Type 

Dim Shared As bg_t bg(3) 

Dim Shared As io_reg bg_pa(3) 
Dim Shared As io_reg bg_pb(3) 
Dim Shared As io_reg bg_pc(3) 
Dim Shared As io_reg bg_pd(3) 

Dim Shared As io_reg bg_refxe(3) 
Dim Shared As io_reg bg_refye(3) 

Dim Shared As io_reg bg_refxi(3) 
Dim Shared As io_reg bg_refyi(3) 

Dim Shared As io_reg win_in 
Dim Shared As io_reg win_out 

Dim Shared As io_reg bld_cnt 
Dim Shared As io_reg bld_alpha 
Dim Shared As io_reg bld_bright 








' SOUND
#define SWEEP_DEC  (1 Shl  3)
#define ENV_INC    (1 Shl 11)
#define CH_LEN     (1 Shl 14)
#define WAVE_64    (1 Shl  5)
#define WAVE_PLAY  (1 Shl  7)
#define NOISE_7    (1 Shl  3)

Type snd_sqr_ch_t 
    As io_reg sweep 
    As io_reg tone 
    As io_reg ctrl 
End Type 

Type snd_wave_ch_t 
    As io_reg wave 
    As io_reg volume 
    As io_reg ctrl 
End Type 

Type snd_noise_ch_t 
    As io_reg env 
    As io_reg ctrl 
End Type 

Dim Shared As snd_sqr_ch_t sqr_ch(1) 
Dim Shared As snd_wave_ch_t wave_ch 
Dim Shared As snd_noise_ch_t noise_ch 

#define CH_SQR1_R   (1 Shl  8)
#define CH_SQR2_R   (1 Shl  9)
#define CH_WAVE_R   (1 Shl 10)
#define CH_NOISE_R  (1 Shl 11)
#define CH_SQR1_L   (1 Shl 12)
#define CH_SQR2_L   (1 Shl 13)
#define CH_WAVE_L   (1 Shl 14)
#define CH_NOISE_L  (1 Shl 15)
#define CH_DMAA_R   (1 Shl  8)
#define CH_DMAA_L   (1 Shl  9)
#define CH_DMAB_R   (1 Shl 12)
#define CH_DMAB_L   (1 Shl 13)
#define CH_SQR1     (1 Shl  0)
#define CH_SQR2     (1 Shl  1)
#define CH_WAVE     (1 Shl  2)
#define CH_NOISE    (1 Shl  3)
#define PSG_ENB     (1 Shl  7)

Dim Shared As io_reg snd_psg_vol 
Dim Shared As io_reg snd_pcm_vol 
Dim Shared As io_reg snd_psg_enb 
Dim Shared As io_reg snd_bias 

Dim Shared As uByte wave_ram(&h1F) 

Dim Shared As Byte snd_fifo_a_0 
Dim Shared As Byte snd_fifo_a_1 
Dim Shared As Byte snd_fifo_a_2 
Dim Shared As Byte snd_fifo_a_3 

Dim Shared As Byte snd_fifo_b_0 
Dim Shared As Byte snd_fifo_b_1 
Dim Shared As Byte snd_fifo_b_2 
Dim Shared As Byte snd_fifo_b_3 







Type dma_ch_t 
    As io_reg src 
    As io_reg dst 
    As io_reg count 
    As io_reg ctrl 
End Type 

Dim Shared As dma_ch_t dma_ch(3) 

#define BTN_A    (1 Shl 0)
#define BTN_B    (1 Shl 1)
#define BTN_SEL  (1 Shl 2)
#define BTN_STA  (1 Shl 3)
#define BTN_R    (1 Shl 4)
#define BTN_L    (1 Shl 5)
#define BTN_U    (1 Shl 6)
#define BTN_D    (1 Shl 7)
#define BTN_RT   (1 Shl 8)
#define BTN_LT   (1 Shl 9)

Type tmr_t 
    As io_reg count 
    As io_reg reload 
    As io_reg ctrl 
End Type 

Dim Shared As tmr_t tmr(3) 

Dim Shared As io_reg r_cnt 
Dim Shared As io_reg sio_cnt 
Dim Shared As io_reg sio_data8 
Dim Shared As io_reg sio_data32 

Dim Shared As io_reg key_input 

Dim Shared As io_reg int_enb 
Dim Shared As io_reg int_ack 
Dim Shared As io_reg wait_cnt 
Dim Shared As io_reg int_enb_m 

Dim Shared As uByte ws_n(3) 
Dim Shared As uByte ws_s(3) 

Dim Shared As uByte ws_n_arm(3) 
Dim Shared As uByte ws_s_arm(3) 

Dim Shared As uByte ws_n_t16(3) 
Dim Shared As uByte ws_s_t16(3) 

Dim Shared As uByte post_boot 

Dim Shared As BOOL io_open_bus 

Declare Function io_read(ADDRESS As ULong) As uByte 

Declare Sub io_write(ADDRESS As ULong , value As uByte) 

Declare Sub trigger_irq(flag As uShort) 

Declare Sub update_ws() 
