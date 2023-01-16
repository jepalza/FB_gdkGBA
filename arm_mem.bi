
' info: https://problemkaputt.de/gbatek.htm#gbamemorymap
'
' General Internal Memory
'  00000000-00003FFF   BIOS - System ROM         (16 KBytes)
'  00004000-01FFFFFF   Not used
'  02000000-0203FFFF   WRAM - On-board Work RAM  (256 KBytes) 2 Wait
'  02040000-02FFFFFF   Not used
'  03000000-03007FFF   WRAM - On-chip Work RAM   (32 KBytes)
'  03008000-03FFFFFF   Not used
'  04000000-040003FE   I/O Registers
'  04000400-04FFFFFF   Not used

'Internal Display Memory

'  05000000-050003FF   BG/OBJ Palette RAM        (1 Kbyte)
'  05000400-05FFFFFF   Not used
'  06000000-06017FFF   VRAM - Video RAM          (96 KBytes)
'  06018000-06FFFFFF   Not used
'  07000000-070003FF   OAM - OBJ Attributes      (1 Kbyte)
'  07000400-07FFFFFF   Not used

'External Memory (Game Pak)

'  08000000-09FFFFFF   Game Pak ROM/FlashROM (max 32MB) - Wait State 0
'  0A000000-0BFFFFFF   Game Pak ROM/FlashROM (max 32MB) - Wait State 1
'  0C000000-0DFFFFFF   Game Pak ROM/FlashROM (max 32MB) - Wait State 2
'  0E000000-0E00FFFF   Game Pak SRAM    (max 64 KBytes) - 8bit Bus width
'  0E010000-0FFFFFFF   Not used

' ininicalizacion ver --> "arm_init()"
' interna
Dim Shared As uByte Ptr bios  '00000000-00003FFF (16k)
Dim Shared As uByte Ptr wram  '02000000-0203FFFF (256k)
Dim Shared As uByte Ptr iwram '03000000-03007FFF (32k)
'Dim Shared As UByte Ptr ioreg '04000000-040003FE   I/O Registers (esta zona se controla en "IO.BAS")
' video
Dim Shared As uByte Ptr pram  '05000000-050003FF (1k)
Dim Shared As uByte Ptr vram  '06000000-06017FFF (96k)
Dim Shared As uByte Ptr oam   '07000000-070003FF (1k)
' externa en cartuchos
Dim Shared As uByte Ptr rom   '08000000-09FFFFFF
Dim Shared As uByte Ptr eeprom'0A000000-0BFFFFFF
Dim Shared As uByte Ptr flash '0C000000-0DFFFFFF
Dim Shared As uByte Ptr sram  '0E000000-0E00FFFF (32k tipico) (max 64k)



' principal, llama a las demas
Declare Function arm_read_(ADDRESS As ULong , offset As uByte) As uByte 

Declare Function arm_readb(ADDRESS As ULong) As uByte 
Declare Function arm_readh(ADDRESS As ULong) As ULong 
Declare Function arm_read(ADDRESS As ULong) As ULong 
Declare Function arm_readb_n(ADDRESS As ULong) As uByte 
Declare Function arm_readh_n(ADDRESS As ULong) As ULong 
Declare Function arm_read_n(ADDRESS As ULong) As ULong 
Declare Function arm_readb_s(ADDRESS As ULong) As uByte 
Declare Function arm_readh_s(ADDRESS As ULong) As ULong 
Declare Function arm_read_s(ADDRESS As ULong) As ULong 

' principal, llama a las demas
Declare Sub arm_write_(ADDRESS As ULong , offset As uByte , value As uByte) 

Declare Sub arm_writeb(ADDRESS As ULong , value As uByte) 
Declare Sub arm_writeh(ADDRESS As ULong , value As uShort) 
Declare Sub arm_write(ADDRESS As ULong , value As ULong) 
Declare Sub arm_writeb_n(ADDRESS As ULong , value As uByte) 
Declare Sub arm_writeh_n(ADDRESS As ULong , value As uShort) 
Declare Sub arm_write_n(ADDRESS As ULong , value As ULong) 
Declare Sub arm_writeb_s(ADDRESS As ULong , value As uByte) 
Declare Sub arm_writeh_s(ADDRESS As ULong , value As uShort) 
Declare Sub arm_write_s(ADDRESS As ULong , value As ULong) 







Dim Shared As ULong DEB_opcode ' depuracion de codigos

Dim Shared As ULong palette_(&h1ff) 

Dim Shared As ULong bios_op 

Dim Shared As LongInt cart_rom_size 
Dim Shared As ULong   cart_rom_mask 

Dim Shared As uShort eeprom_idx 

Enum access_type_e 
    _NON_SEQ,
    _SEQUENTIAL
End Enum 

