

#define CPU_FREQ_HZ       16777216 ' 16.78mhz
#define SND_FREQUENCY     32768
#define SND_CHANNELS      2
#define SND_SAMPLES       512
#define SAMP_CYCLES       (CPU_FREQ_HZ / SND_FREQUENCY)
#define BUFF_SAMPLES      ((SND_SAMPLES) * 16 * 2)
#define BUFF_SAMPLES_MSK  ((BUFF_SAMPLES) - 1)

Dim Shared As Byte fifo_a(&h1f) 
Dim Shared As Byte fifo_b(&h1f) 

Dim Shared As uByte fifo_a_len 
Dim Shared As uByte fifo_b_len 

Type snd_ch_state_t 
    As BOOL     phase        'Square 1/2 only
    As UShort 	 lfsr         'Noise only
    As Double   samples      'All
    As Double   length_time  'All
    As Double   sweep_time   'Square 1 only
    As Double   env_time     'All except Wave
End Type 

Dim Shared As snd_ch_state_t snd_ch_state(3) 

Dim Shared As uByte wave_position 
Dim Shared As uByte wave_samples 

Declare Sub wave_reset() 

Declare Sub sound_buffer_wrap() 

Declare Sub sound_mix cdecl(ByVal data_ As ubyte Ptr , ByVal stream As uByte Ptr , ByVal len_ As Long) 
Declare Sub sound_clock(cycles As ULong) 

Declare Sub fifo_a_copy() 
Declare Sub fifo_b_copy() 

Declare Sub fifo_a_load() 
Declare Sub fifo_b_load() 
