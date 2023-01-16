

#define PSG_MAX   &h7f
#define PSG_MIN  -&h80

#define SAMP_MAX   &h1ff
#define SAMP_MIN  -&h200

'How much time a single sample takes (in seconds)
#define SAMPLE_TIME  (1.0 / (SND_FREQUENCY))

Static Shared As Double duty_lut(3)   = { 0.125, 0.250, 0.500, 0.750 } 
Static Shared As Double duty_lut_i(3) = { 0.875, 0.750, 0.500, 0.250 } 

Function square_sample(ch As uByte) As Byte 
    If (snd_psg_enb.w And (CH_SQR1 Shl ch))=0 Then return 0 

    Dim As UByte  sweep_time = (sqr_ch(ch).sweep.w Shr  4) And &h7 
    Dim As UByte  duty       = (sqr_ch(ch).tone.w  Shr  6) And &h3 
    Dim As UByte  env_step   = (sqr_ch(ch).tone.w  Shr  8) And &h7 
    Dim As UByte  envelope   = (sqr_ch(ch).tone.w  Shr 12) And &hf 
    Dim As UByte  snd_len    = (sqr_ch(ch).tone.w  Shr  0) And &h3f 
    Dim As UShort freq_hz    = (sqr_ch(ch).ctrl.w  Shr  0) And &h7ff 

    'Actual frequency in Hertz
    Dim As Double frequency = 131072 / (2048 - freq_hz) 

    'Full length of the generated wave (if enabled) in seconds
    Dim As Double length = (64 - snd_len) / 256.0 

    'Frquency sweep change interval in seconds
    Dim As Double sweep_interval = 0.0078 * (sweep_time + 1) 

    'Envelope volume change interval in seconds
    Dim As Double envelope_interval = env_step / 64.0 

    'Numbers of samples that a single cycle (wave phase change 1 -> 0) takes at output sample rate
    Dim As Double cycle_samples = SND_FREQUENCY / frequency 

    'Length reached check (if so, just disable the channel and return silence)
    if (sqr_ch(ch).ctrl.w And CH_LEN)<>0 Then 
        snd_ch_state(ch).length_time += SAMPLE_TIME 
        if (snd_ch_state(ch).length_time >= length) Then 
            'Disable channel
            snd_psg_enb.w And= INV(CH_SQR1 Shl ch)
            'And return silence
            return 0 
        EndIf
    EndIf
  

    'Frequency sweep (Square 1 channel only)
    if (ch = 0) Then 
        snd_ch_state(0).sweep_time += SAMPLE_TIME 
        if (snd_ch_state(0).sweep_time >= sweep_interval) Then 
            snd_ch_state(0).sweep_time -= sweep_interval 
            'A Sweep Shift of 0 means that Sweep is disabled
            Dim As UByte sweep_shift = sqr_ch(0).sweep.w And 7 
            if (sweep_shift) Then 
                Dim As ULong disp = freq_hz Shr sweep_shift 
                if (sqr_ch(0).sweep.w And SWEEP_DEC)<>0 Then 
                    freq_hz -= disp 
                Else
                    freq_hz += disp
                EndIf

                if (freq_hz < &h7ff) Then 
                    'Update frequency
                    sqr_ch(0).ctrl.w And= INV(&h7ff) 
                    sqr_ch(0).ctrl.w Or= freq_hz 
                Else
                    'Disable channel
                    snd_psg_enb.w And= INV(CH_SQR1) 
                EndIf
            EndIf
        EndIf
    EndIf
  

    'Envelope volume
    if (env_step) Then 
        snd_ch_state(ch).env_time += SAMPLE_TIME 
        if (snd_ch_state(ch).env_time >= envelope_interval) Then 
            snd_ch_state(ch).env_time -= envelope_interval 
            if (sqr_ch(ch).tone.w And ENV_INC)<>0 Then 
                if (envelope < &hf) Then envelope+=1  
            Else
                if (envelope > &h0) Then envelope-=1  
            EndIf
            sqr_ch(ch).tone.w And= INV(&hf000) 
            sqr_ch(ch).tone.w Or= envelope Shl 12 
        EndIf
    EndIf

    'Phase change (when the wave goes from Low to High or High to Low, the Square Wave pattern)
    snd_ch_state(ch).samples+=1  

    if (snd_ch_state(ch).phase) Then 
        '1 -> 0
        Dim As Double phase_change = cycle_samples * duty_lut(duty) 
        if (snd_ch_state(ch).samples >  phase_change) Then 
            snd_ch_state(ch).samples -= phase_change 
            snd_ch_state(ch).phase = FALSE 
        EndIf
    Else
        '0 -> 1
        Dim As Double phase_change = cycle_samples * duty_lut_i(duty)
        if (snd_ch_state(ch).samples >  phase_change) Then 
            snd_ch_state(ch).samples -= phase_change 
            snd_ch_state(ch).phase = TRUE 
        EndIf
    EndIf

    return IIf(snd_ch_state(ch).phase<>0, (envelope / 15.0) * PSG_MAX, (envelope / 15.0) * PSG_MIN) 
End Function

Function wave_sample() As Byte 
    If ( ((snd_psg_enb.w And CH_WAVE)<>0) And ((wave_ch.wave.w And WAVE_PLAY)<>0) )=0 Then return 0 

    Dim As UByte  snd_len = (wave_ch.volume.w Shr  0) And &hff 
    Dim As UByte  volume  = (wave_ch.volume.w Shr 13) And &h7 
    Dim As UShort freq_hz = (wave_ch.ctrl.w   Shr  0) And &h7ff 

    'Actual frequency in Hertz
    Dim As Double frequency = 2097152 / (2048 - freq_hz) 

    'Full length of the generated wave (if enabled) in seconds
    Dim As Double length = (256 - snd_len) / 256.0 

    'Numbers of samples that a single "cycle" (all entries on Wave RAM) takes at output sample rate
    Dim As Double cycle_samples = SND_FREQUENCY / frequency 

    'Length reached check (if so, just disable the channel and return silence)
    if (wave_ch.ctrl.w And CH_LEN) Then 
        snd_ch_state(2).length_time += SAMPLE_TIME 
        if (snd_ch_state(2).length_time >= length) Then 
            'Disable channel
            snd_psg_enb.w And=  INV(CH_WAVE) 
            'And return silence
            return 0 
        EndIf
    EndIf
  
    snd_ch_state(2).samples+=1  

    if (snd_ch_state(2).samples >= cycle_samples) Then 
        snd_ch_state(2).samples -= cycle_samples 
			wave_samples-=1
        if (wave_samples) Then 
            wave_position = (wave_position + 1) And &h3f 
        Else
            wave_reset()
        EndIf
    EndIf
  
    Dim As Byte samp = IIf ((wave_position And 1)<>0 _
        , ((wave_ram((wave_position Shr 1) And &h1f) Shr 0) And &hf) - 8 _
        , ((wave_ram((wave_position Shr 1) And &h1f) Shr 4) And &hf) - 8) 

    Select Case As Const (volume)  
    	case 0  
    		samp     = 0  'Mute
    	case 1  
    		samp Shr = 0  '100%
    	case 2  
    		samp Shr = 1  '50%
    	case 3  
    		samp Shr = 2  '25%
    	case else  
    		samp = (samp Shr 2) * 3  '75%
    End Select

    return IIf(samp >= 0 _
        , (samp /  7.0) * PSG_MAX _
        , (samp / -8.0) * PSG_MIN) 
End Function

Function noise_sample() As Byte 
    If  (snd_psg_enb.w And CH_NOISE)=0 Then return 0 

    Dim As UByte env_step = (noise_ch.env.w  Shr  8) And &h7 
    Dim As UByte envelope = (noise_ch.env.w  Shr 12) And &hf 
    Dim As UByte snd_len  = (noise_ch.env.w  Shr  0) And &h3f 
    Dim As UByte freq_div = (noise_ch.ctrl.w Shr  0) And &h7 
    Dim As UByte freq_rsh = (noise_ch.ctrl.w Shr  4) And &hf 

    'Actual frequency in Hertz
    Dim As Double frequency = IIf(freq_div _
        , (524288 / freq_div) Shr (freq_rsh + 1) _
        , (524288 *        2) Shr (freq_rsh + 1)) 

    'Full length of the generated wave (if enabled) in seconds
    Dim As Double length = (64 - snd_len) / 256.0 

    'Envelope volume change interval in seconds
    Dim As Double envelope_interval = env_step / 64.0 

    'Numbers of samples that a single cycle (pseudo-random noise value) takes at output sample rate
    Dim As Double cycle_samples = SND_FREQUENCY / frequency 

    'Length reached check (if so, just disable the channel and return silence)
    if (noise_ch.ctrl.w And CH_LEN)<>0 Then 
        snd_ch_state(3).length_time += SAMPLE_TIME 
        if (snd_ch_state(3).length_time >= length) Then 
            'Disable channel
            snd_psg_enb.w And= INV(CH_NOISE) 
            'And return silence
            return 0 
        EndIf
    EndIf
  

    'Envelope volume
    if (env_step) Then 
        snd_ch_state(3).env_time += SAMPLE_TIME 
        if (snd_ch_state(3).env_time >= envelope_interval) Then 
            snd_ch_state(3).env_time -= envelope_interval 
            if (noise_ch.env.w And ENV_INC)<>0 Then 
                if (envelope < &hf) Then envelope+=1  
            Else
                if (envelope > &h0) Then envelope-=1  
            EndIf
            noise_ch.env.w And= INV(&hf000) 
            noise_ch.env.w Or= envelope Shl 12 
        EndIf
    EndIf
  
    Dim As UByte carry = snd_ch_state(3).lfsr And 1 
    snd_ch_state(3).samples+=1  

    if (snd_ch_state(3).samples >= cycle_samples) Then 
        snd_ch_state(3).samples -= cycle_samples 
        snd_ch_state(3).lfsr Shr = 1 
        Dim As UByte high = (snd_ch_state(3).lfsr And 1) Xor carry 
        if (noise_ch.ctrl.w And NOISE_7)<>0 Then 
            snd_ch_state(3).lfsr Or= (high Shl  6) 
        Else
            snd_ch_state(3).lfsr Or= (high Shl 14)
        EndIf
    EndIf

    return IIf( carry _
        , (envelope / 15.0) * PSG_MAX _
        , (envelope / 15.0) * PSG_MIN )
        
End Function








Dim Shared As ULong snd_cur_play= 0 
Dim Shared As ULong snd_cur_write= &h200 

Sub wave_reset() 
    if (wave_ch.wave.w And WAVE_64) Then 
        '64 samples (at 4 bits each, uses both banks so initial position is always 0)
        wave_position = 0 
        wave_samples  = 64 
    Else
        '32 samples (at 4 bits each, bank selectable through Wave Control register)
        wave_position = (wave_ch.wave.w Shr 1) And &h20 
        wave_samples  = 32 
    EndIf
End Sub

Sub sound_buffer_wrap() 
    /'
     * This prevents the cursor from overflowing
     * Call after some time (like per frame, or per second...)
     '/
    if ((snd_cur_play / BUFF_SAMPLES) = (snd_cur_write / BUFF_SAMPLES)) Then 
        snd_cur_play  And= BUFF_SAMPLES_MSK 
        snd_cur_write And= BUFF_SAMPLES_MSK 
    EndIf
  
End Sub


' rutina llamada desde SDL 
Dim Shared As Short snd_buffer(BUFF_SAMPLES-1) 

Sub sound_mix cdecl(ByVal data_ As ubyte Ptr , ByVal stream As uByte Ptr , ByVal len_ As Long) 
    Dim As UShort i 
	' nota: argumento DATA_ no se emplea, pero es necesario para SDL, sino, da error
	
    for  i = 0 To len_-1 Step 4     
        *(stream + (i Or 0) +1) = ((snd_buffer(snd_cur_play  And BUFF_SAMPLES_MSK) Shl 6) And &hFF00) Shr 8
        *(stream + (i Or 0) +0) = ((snd_buffer(snd_cur_play  And BUFF_SAMPLES_MSK) Shl 6) And &hFF)
        snd_cur_play+=1
        *(stream + (i Or 2) +1) = ((snd_buffer(snd_cur_play  And BUFF_SAMPLES_MSK) Shl 6) And &hFF00) Shr 8
        *(stream + (i Or 2) +0) = ((snd_buffer(snd_cur_play  And BUFF_SAMPLES_MSK) Shl 6) And &hFF)
        snd_cur_play+=1 
    Next

    'Avoid desync between the Play cursor and the Write cursor
    snd_cur_play += ((snd_cur_write - snd_cur_play) Shr 8) And INV(1) 
End Sub

Sub fifo_a_copy() 
    if (fifo_a_len + 4) > &h20 Then Exit sub  'FIFO A full

    fifo_a(fifo_a_len ) = snd_fifo_a_0  :fifo_a_len+=1
    fifo_a(fifo_a_len ) = snd_fifo_a_1  :fifo_a_len+=1
    fifo_a(fifo_a_len ) = snd_fifo_a_2  :fifo_a_len+=1
    fifo_a(fifo_a_len ) = snd_fifo_a_3  :fifo_a_len+=1
End Sub

Sub fifo_b_copy() 
    if (fifo_b_len + 4) > &h20 Then Exit Sub  'FIFO B full

    fifo_b(fifo_b_len ) = snd_fifo_b_0  :fifo_b_len+=1
    fifo_b(fifo_b_len ) = snd_fifo_b_1  :fifo_b_len+=1
    fifo_b(fifo_b_len ) = snd_fifo_b_2  :fifo_b_len+=1
    fifo_b(fifo_b_len ) = snd_fifo_b_3  :fifo_b_len+=1
End Sub

Dim Shared As Byte fifo_a_samp 
Dim Shared As Byte fifo_b_samp 

Sub fifo_a_load() 
    if (fifo_a_len) Then 
        fifo_a_samp = fifo_a(0) 
        fifo_a_len-=1  
        Dim As UByte i 
        for i = 0 To fifo_a_len-1         
            fifo_a(i) = fifo_a(i + 1) 
        Next
    EndIf
End Sub

Sub fifo_b_load() 
    if (fifo_b_len) Then 
        fifo_b_samp = fifo_b(0) 
        fifo_b_len-=1  
        Dim As UByte i 
        for i = 0 To fifo_b_len-1         
            fifo_b(i) = fifo_b(i + 1) 
        Next
    EndIf
End Sub

Dim Shared As ULong snd_cycles= 0 

static Shared As Long psg_vol_lut(7) = { &h000, &h024, &h049, &h06d, &h092, &h0b6, &h0db, &h100 } 
Static Shared As Long psg_rsh_lut(3) = { &ha, &h9, &h8, &h7 } 

Function clip(value As Long) As Short 
    if (value > SAMP_MAX) Then value = SAMP_MAX 
    if (value < SAMP_MIN) Then value = SAMP_MIN 

    return value
End Function


Sub sound_clock(cycles As ULong) 
    snd_cycles += cycles 

    Dim As Short samp_pcm_l = 0 
    Dim As Short samp_pcm_r = 0 

    Dim As Short samp_ch4 = (fifo_a_samp Shl 1) Shr IIf((snd_pcm_vol.w And 4)=0 ,1,0)
    Dim As Short samp_ch5 = (fifo_b_samp Shl 1) Shr IIf((snd_pcm_vol.w And 8)=0 ,1,0)

    if (snd_pcm_vol.w And CH_DMAA_L) Then samp_pcm_l = clip(samp_pcm_l + samp_ch4) 
    if (snd_pcm_vol.w And CH_DMAB_L) Then samp_pcm_l = clip(samp_pcm_l + samp_ch5) 

    if (snd_pcm_vol.w And CH_DMAA_R) Then samp_pcm_r = clip(samp_pcm_r + samp_ch4) 
    if (snd_pcm_vol.w And CH_DMAB_R) Then samp_pcm_r = clip(samp_pcm_r + samp_ch5) 

    while (snd_cycles >= SAMP_CYCLES)  
        Dim As Short samp_ch0 = square_sample(0) 
        Dim As Short samp_ch1 = square_sample(1) 
        Dim As Short samp_ch2 = wave_sample() 
        Dim As Short samp_ch3 = noise_sample() 

        Dim As Long samp_psg_l = 0 
        Dim As Long samp_psg_r = 0 

        if (snd_psg_vol.w And CH_SQR1_L)  Then samp_psg_l = clip(samp_psg_l + samp_ch0) 
        if (snd_psg_vol.w And CH_SQR2_L)  Then samp_psg_l = clip(samp_psg_l + samp_ch1) 
        if (snd_psg_vol.w And CH_WAVE_L)  Then samp_psg_l = clip(samp_psg_l + samp_ch2) 
        if (snd_psg_vol.w And CH_NOISE_L) Then samp_psg_l = clip(samp_psg_l + samp_ch3) 

        if (snd_psg_vol.w And CH_SQR1_R)  Then samp_psg_r = clip(samp_psg_r + samp_ch0) 
        if (snd_psg_vol.w And CH_SQR2_R)  Then samp_psg_r = clip(samp_psg_r + samp_ch1) 
        if (snd_psg_vol.w And CH_WAVE_R)  Then samp_psg_r = clip(samp_psg_r + samp_ch2) 
        if (snd_psg_vol.w And CH_NOISE_R) Then samp_psg_r = clip(samp_psg_r + samp_ch3) 

        samp_psg_l  *= psg_vol_lut((snd_psg_vol.w Shr 4) And 7) 
        samp_psg_r  *= psg_vol_lut((snd_psg_vol.w Shr 0) And 7) 

        samp_psg_l Shr = psg_rsh_lut((snd_pcm_vol.w Shr 0) And 3) 
        samp_psg_r Shr = psg_rsh_lut((snd_pcm_vol.w Shr 0) And 3) 

        snd_buffer(snd_cur_write And BUFF_SAMPLES_MSK) = clip(samp_psg_l + samp_pcm_l) :snd_cur_write+=1
        snd_buffer(snd_cur_write And BUFF_SAMPLES_MSK) = clip(samp_psg_r + samp_pcm_r) :snd_cur_write+=1

        snd_cycles -= SAMP_CYCLES 
    Wend
    
End Sub