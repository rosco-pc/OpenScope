{OpenScope}


CON
  _clkmode = xtal1 + pll16x                                                     ' Standard clock mode * crystal frequency = 100 MHz
  _xinfreq = 6_250_000

' PROPELLER PORTS
'---------------------------------------------------------------------------------------------------------------------------------
  AUX                           = 10                                            ' Offest for expansion card pins
  ADC_CLK                       = 26                                            ' ADC Clock
  MUX                           = 27                                            ' Multiplex Channels 1 or 2 onto D0-D9
  SCL                           = 28                                            ' I2C Clock
  SDA                           = 29                                            ' I2C Data
  TX                            = 30                                            ' Serial Transmit
  RX                            = 31                                            ' Serial Receive

' I2C DEVICE ADDRESSES
'----------------------------------------------------------------------------------------------------------------------------------
  INT_EEPROM                    = $50 << 1                                      ' Internal EEPROM
  EXP_EEPROM                    = $51 << 1                                      ' Expansion EEPROM
  PCA9538                       = $70 << 1                                      ' PCA9538 I/O Expander

' PCA9538 definitions
'----------------------------------------------------------------------------------------------------------------------------------

  PCA9538_IN                    = 0                                             ' Input Register (read-only), read level on input pins
  PCA9538_OUT                   = 1                                             ' Output Register, determines level of output pins
  PCA9538_POL                   = 2                                             ' Polarity Inversion Register, reverse polarity of output pins
  PCA9538_DIR                   = 3                                             ' Configuration Register, determines I/O pins direction: 0=output, 1=input
  
  CH1_OFF                       = 0                                             ' Channel 1 Offset
  CH2_OFF                       = 1                                             ' Channel 2 Offset
  CH1_DIV                       = 2                                             ' Channel 1 Probe division
  CH2_DIV                       = 3                                             ' 
  CH1_RANGE                     = 4
  CH2_RANGE                     = 5
  NEG_SUPPLY                    = 6
  CARD                          = 7
  
  PCA9538_DIR_DEF               = %1100_1111                                    ' 1 Indicates pins externally pulled low
  PCA9538_OUT_DEF               = %1100_1111                                    ' Initial 

' NUMERICALLY CONTROLLED OSCILLATOR
'----------------------------------------------------------------------------------------------------------------------------------
  NCO                           = %00100_000 << 23 + 1 << 9 + ADC_CLK           ' Single Ended NCO on ADC Clock
  FRQ_25MHZ                     = $4000_0000                                    ' 25MHz Clock Frequency
  FRQ_12_5MHZ                   = FRQ_25MHZ >> 1                                ' 12.5MHz Clock Frequency
  FRQ_6_25MHZ                   = FRQ_12_5MHZ >> 1                              ' 6.25MHz Clock Frequency
  FRQ_3_125MHZ                  = FRQ_6_25MHZ >> 1                              ' 3.125MHz Clock Frequency
  FRQ_1_5625MHZ                 = FRQ_3_125MHZ >> 1                             ' 1.5625MHz Clock Frequency
          
' SYSTEM 
'----------------------------------------------------------------------------------------------------------------------------------
  BAUD   = 230_400                                                              ' Baud Rate
  BUF_SZ = 512                                                                  ' Buffer Size

' TIMING
'----------------------------------------------------------------------------------------------------------------------------------
  CLK_FREQ                      = ((_clkmode - xtal1) >> 6) * _xinfreq
  MS_001                        = CLK_FREQ / 1_000

' OpenScope protocol as specified in doc/openscpoc_protocol_0_1.txt
' ----------------------------------------------------------------------------------------------------------------------------------
' Commands
' ----------------------------------------------------------------------------------------------------------------------------------
  CMD_INIT                        = $00                                             ' Reset device
  CMD_RUN                         = $01                                             ' Start sampling and sending data
  CMD_SET                         = $02                                             ' Get channel data
  CMD_GET                         = $03                                             ' Get channel data
  CMD_AUX                         = $04                                             ' Set AUX configuration
' ----------------------------------------------------------------------------------------------------------------------------------
' Functions for SET cmd
' ----------------------------------------------------------------------------------------------------------------------------------
  ACTIVATE                    = 1
  OFFSET                      = 2
  TRIGGER                     = 3
  FREQUENCY                   = 4
  
  CHANNEL_PROP                = 8
  
VAR
  Byte  buff[BUF_SZ]                                                            ' Buffer
  Byte  str[32]                                                                 ' String buffer
  Byte  channel[31*CHANNEL_PROP]                                                ' Channel description
   
OBJ
  I2C     : "Basic_I2C_Driver"                                                  ' I2C Communications
'  Comm    : "FastFullDuplexSerial"                                              ' Serial Communication to PC
  Comm    : "fullDuplexSerial4port"                                             ' Serial Communication to PC
  Num     : "Numbers"                                                           ' Number conversion routines
  
PUB Main
  Init
  CommandLoop

PRI Init 
  ' Initialize scope in default configuration: 2 analog channels, AUX disabled
  DIRA[0..19]~                                                                  ' Set Channels as Inputs
  DIRA[ADC_CLK]~~                                                               ' Set ADC Clock as Output
  DIRA[MUX]~~                                                                   ' Set Multiplex as Output
  OUTA[MUX]~~                                                                   ' D0-D9 Ch1, D10-D19 Ch2
  
  I2C.Initialize(SCL)                                                           ' Start I2C Bus
  
  I2C.WriteByte(SCL, PCA9538, PCA9538_OUT, PCA9538_OUT_DEF)                     ' Externally pulled low pins get high level, all other low
  I2C.WriteByte(SCL, PCA9538, PCA9538_DIR, PCA9538_DIR_DEF)                     ' Define externally pulled low pins as input (floating), all others as output
  
  channel[0] := 132                                                             ' Channel 1, Active, Analog, 4 bytes info
  channel[5] := 10
  channel[6] := 0
  channel[7] := 0
  channel[CHANNEL_PROP] := 132                                                  ' Channel 2, Active, Analog, 4 bytes info
  channel[CHANNEL_PROP+5] := 179
  channel[CHANNEL_PROP+6] := 0
  channel[CHANNEL_PROP+7] := 0
    
  Comm.init
  Comm.addPort(0, RX, TX, -1, -1, 0, 0, BAUD)                                   ' Start Serial Bus
  Pause(100)                                                                    ' Wait for Serial Buss to settle

  getDevice(@str)
  Comm.str(0, str)                                                              ' Send name of device and AUX identifiers

  CTRA := NCO                                                                   ' Start ADC Clock
  FRQA := FRQ_25MHZ                                                             ' 25MHz Clock

PRI CommandLoop | data
  Repeat
     Case Comm.rxcheck(0)                                                       ' Get command
      CMD_INIT:                                                                     ' Reset scope
        Init
      CMD_RUN:                                                                      ' Start sampling
        Sample
      CMD_SET:                                                                      ' Set channel data
        data := Comm.rxcheck(0)                                                 ' Get channel no           
        setData(data)                     
      CMD_GET:                                                                      ' Return Meta Data
        data := Comm.rxcheck(0)                                                 ' Get channel no           
        getData(data)                                                           ' Get channel info
      CMD_AUX:                                                                      ' Define AUX channells
        data := Comm.rxcheck(0)                                                 ' Get no of channels          
        setAUX(data)                     

PRI Pause(ms) | t
'' Delay program in milliseconds

  t := cnt - 1088                                                               ' Sync with system counter
  repeat (ms #> 0)
    waitcnt(t += MS_001)

PRI Flush(data, len) | index
'' Flush contents of buffer
  BYTEFILL(data, 0, len)
    
PRI Set_PCA9538Pin(pin) | out
{{ Turn on relay controlled by PCA9538 pin Pin
   Do this using the following rules:
    * Externally pulled low pins: set pin as output
    * Output pins: set pin output high
}}

  |< pin                                                                        ' Get pin number
  IF PCA9538_DIR_DEF & pin                                                      ' Externally pulled low pin
    out := I2C.ReadByte(SCL, PCA9538, PCA9538_DIR)                              ' Read Configuration Register
    out &= !pin                                                                 ' set pin Pin as output (0)
    I2C.WriteByte(SCL, PCA9538, PCA9538_DIR, out)                               ' Write back to Configuration Register
    pause (50)                                                                  ' wait for 50 ms to let relay settle
  ELSEIFNOT pin > 128                                                           ' Valid Output pin (0..7) 
    out := I2C.ReadByte(SCL, PCA9538, PCA9538_OUT)                              ' Read Output Register
    out |= pin                                                                  ' set pin Pin high
    I2C.WriteByte(SCL, PCA9538, PCA9538_OUT, out)                               ' Write back to Output Register
 
PRI Rel_PCA9538Pin(pin) | out  
{{ Turn off relay controlled by PCA9538 pin Pin
   Do this using the following rules:
    * Externally pulled low pins: set Pin as input
    * Output pins: set Pin output low
}}

  |< pin                                                                        ' Get pin number
  IF PCA9538_DIR_DEF & pin                                                      ' Externally pulled low pin
    out := I2C.ReadByte(SCL, PCA9538, PCA9538_DIR)                              ' Read Configuration Register
    out |= pin                                                                  ' set pin Pin as input (1)
    I2C.WriteByte(SCL, PCA9538, PCA9538_DIR, out)                               ' Write back to Configuration Register
    pause (50)                                                                  ' wait for 50 ms to let relay settle
  ELSEIFNOT pin > 128                                                           ' Valid Output pin (0..7)
    out := I2C.ReadByte(SCL, PCA9538, PCA9538_OUT)                              ' Read Output Register
    out &= !pin                                                                 ' set pin pin low
    I2C.WriteByte(SCL, PCA9538, PCA9538_OUT, out)                               ' Write back to Output Register

PRI setData(data) | ch, val, index, start, stop
  ch := data >> 3
  CASE data & 7
    ACTIVATE: 
      val := Comm.rxcheck(0)
      channel[ch*CHANNEL_PROP] &= val << 7
      ' do the actual activation of channel
      'IF ch > 2 AND ch < 32
      '  Set_PCA9538Pin(CARD)
      '  start := AUX + channel[ch*CHANNEL_PROP+6] >> 4
      '  stop  := AUX + channel[ch*CHANNEL_PROP+6] & $F
      '  DIRA[start..stop] :=  
    OFFSET:
      val := Comm.rxcheck(0)
      channel[ch*CHANNEL_PROP+1] &= $F3
      channel[ch*CHANNEL_PROP+1] &= (val & 3)<<2
      ' Do actual offset setting
    TRIGGER:
      REPEAT index FROM 1 TO 4
        data := Comm.rxcheck(0)
        val += data
        val <<= 8
        channel[ch*CHANNEL_PROP+index] := data
    FREQUENCY:
      REPEAT index FROM 1 TO 4
        data := Comm.rxcheck(0)
        val += data
        val <<= 8
        channel[ch*CHANNEL_PROP+index] := data
    
    
PRI setAUX(count) | ch
  REPEAT count 
    ch := Comm.rxcheck(0)
    IF ch > 2 AND ch < 32
      channel[ch]   := Comm.rxcheck(0)
      channel[ch+5] := Comm.rxcheck(0)
      channel[ch+6] := Comm.rxcheck(0)
      channel[ch+7] := Comm.rxcheck(0)

PRI getData(data) | index
  CASE data
    0:
      REPEAT index FROM 0 TO 31
        sendChannel(index)
    1..32:
      sendChannel(data-1)
    255:
      I2C.ReadPage(SCL, INT_EEPROM, $FF80, @buff, 128)                           ' Read device data
      sendCalibration(@buff+16)

PRI getDevice(ptr)
  Flush(ptr, 32)                                                                ' Flush string
  I2C.ReadPage(SCL, INT_EEPROM, $FF80, ptr, 16)                                 ' Read device name
  I2C.ReadPage(SCL, EXP_EEPROM, 0, ptr, 16)                                     ' Read expansion card eeprom
  
PRI sendChannel(index)
  Comm.tx(0, channel[index])
  REPEAT channel[index] & 3
    Comm.tx(0, channel[++index])
  
PRI sendCalibration(ptr) | index
  REPEAT index FROM 0 TO 111
    Comm.tx(0, ptr[index++])
    
PRI Sample
  RETURN
  
PRI Tuple_out(addr, len) | index
  Comm.tx(0, "(")
  index := 0
  REPEAT len-1
    Comm.Str(0, Num.toStr(LONG[@buff + index*4], Num#IHEX8))
    Comm.tx(0, ",")
    index++
  Comm.Str(0, Num.toStr(LONG[@buff + index*4], Num#IHEX8))
  Comm.tx(0, ")")
  
DAT

  DEVNAME      BYTE "OpenScope v0.1",0
  VERSION      BYTE "0.1", 0
  CALIBRATION  BYTE "calibration=",0
  CHANNELS     BYTE 2
  EXPVERSION   WORD 0

{{
┌─────────────────────────────────────────────────────────────────────────────┐
│                        TERMS OF USE: MIT License                            │
├─────────────────────────────────────────────────────────────────────────────┤
│Permission is hereby granted, free of charge, to any person obtaining a copy │
│of this software and associated documentation files (the "Software"), to deal│
│in the Software without restriction, including without limitation the rights │
│to use, copy, modify, merge, publish, distribute, sublicense, and/or sell    │
│copies of the Software, and to permit persons to whom the Software is        │
│furnished to do so, subject to the following conditions:                     │
│                                                                             │
│The above copyright notice and this permission notice shall be included in   │
│all copies or substantial portions of the Software.                          │
│                                                                             │
│THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   │
│IMPLIED, INCLUDING BUT NOT LIMITED TO THE │WARRANTIES OF MERCHANTABILITY,    │
│FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  │
│AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       │
│LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,ARISING FROM, │
│OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN    │ 
│THE SOFTWARE.                                                                │
└─────────────────────────────────────────────────────────────────────────────┘
}}
        
