{{  FullDuplexSerial4portPlus version 1.01
  - Tracy Allen (TTA)   (c)22-Jan-2011   MIT license, see end of file for terms of use.  Extends existing terms of use.
  - Can open up to 4 independent serial ports, using only one pasm cog for all 4.
  - Supports flow control and open and inverted baud modes
  - Individually configurable tx and rx buffers for all 4 ports, any size, set in CONstants section at compile time
  - Part of the buffer fits within the object's hub footprint, but even so the object is restartable
  - Buffers are DAT type variables, therefore a single instance of the object can be accessed throughout a complex project.

  - Modified from Tim Moore's pcFullDuplexSerial4fc, with further motivation and ideas from Duane Degn's pcFullDuplexSerial4fc128
  - Changes and bug fixes include:
    - Flow control is now operational when called for, with correct polarity (bug correction)
    - Jitter is reduced, unused ports are properly skipped over (bug correction), operation speed is increased.
    - Stop bit on reception is now checked, and if there is a framing error, the byte is not put in the buffer.
    - Buffer sizes are arbitrary, each port separate rx & tx up to available memory
       Changes in pasm and in Spin methods to accomodate larger buffers, major reorganization of DAT section.
    - Added strn method for counted string, and rxHowFull method for buffer size.
    - Cut out most of the format methods such as DEC and HEX, expecting those to be their own object calling rx, tx, str and strn methods.
      See companion object DataIO4port.spin in order to maintain compatibility with methods in the original pcFullDuplexSerial4fc.

  - 1v01
    - init method returns pointer @rxsize, for data buffers and data structure.
  - 1v00
    - documentation
  - 0v91
    - restored DEFAULTTHRESHOLD constant
    - made default buffer sizes in the DAT section rather than init
    - removed the numeric methods to their own companion object, dataIO4port.
  - 0v3
    - first public release with the jitter and flow control issues fixed, and large buffers.

  Links:
  Development of this version:
  --- http://forums.parallax.com/showthread.php?137349-yet-another-variant-fullDuplexSerial4portplus
  Tim Moore's original pcFullDuplexSerial4fc and updates to allow flow control:
  --- http://forums.parallaxinc.com/forums/default.aspx?f=25&p=1&m=273291#m276667
  --- http://obex.parallax.com/objects/340/      7/24/08 version
  --- http://forums.parallaxinc.com/forums/default.aspx?f=25&p=1&m=349173    8/14/08 update, flow polarity correction, not in obex
  Duane Degn's thread, larger 128 or 512 byte buffers and reusing buffer space, discussion of issues
  --- http://forums.parallax.com/showthread.php?129714-Tim-Moore-s-pcFullDuplexSerial4FC-with-larger-%28512-byte%29-rx-buffer
  Juergen Buchmueller, 2 port trimmed down version
  --- http://forums.parallax.com/showthread.php?128184-Serial-Objects-for-SPIN-Programming&p=967075&viewfull=1#post967075
  Serial Mirror, single port but same idea regarding buffers in the DATa space
  --- http://forums.parallax.com/showthread.php?94311-SerialMirror-A-FullDuplexSerial-enhancement
  --- http://obex.parallax.com/objects/189/
  Re baud rates attainable, hiccups:
  --- http://forums.parallaxinc.com/forums/default.aspx?f=25&p=1&m=282923#m282978
  --- http://forums.parallaxinc.com/forums/default.aspx?f=25&p=1&m=334784
  --- http://forums.parallax.com/showthread.php?120868-FullDuplexSerial-hiccups
  Jitter, discussions of jitter in different Prop serial programs, PhiPi's development of PBnJ full duplex:
  --- http://forums.parallax.com/showthread.php?129776-Anybody-aware-of-high-accuracy-(0.7-or-less)-serial-full-duplex-driver
  --- http://forums.parallax.com/showthread.php?136431-Serial-objects-question
  Humanoido's catalog of serial port objects
  --- http://forums.parallax.com/showthread.php?128184-Serial-Objects-for-SPIN-Programming

Tim Moore's release notes follow...   Also note by Duane Degn.
Not all these comments apply to  FullDuplexSerial4port.
}}
''******************************************************************
''*  Based on                                                      *
''*  Full-Duplex Serial Driver v1.1                                *
''*  Author: Chip Gracey                                           *
''*  Copyright (c) 2006 Parallax, Inc.                             *
''*  See end of file for terms of use.                             *
''*                                                                *
''*  Tim Moore 2008                                                *
''*   Modified to support 4 serial ports                           *
''*   It should run 1 port faster than FullDuplexSerial or run     *
''*   up to 4 ports                                                *
''*   Merged 64 byte rx buffer change                              *
''*   Merged Debug_PC (Jon Williams)                               *
''*   (TTA) cut the numeric methods, see dataIO4port.spin          *
''*   to maintain compatibility with pcFullDuplexSerial4fc         *
''*   or use other numeric methods such as "simpleNumbers"         *
''*   Uses DAT rather than VAR so can be used in multiple objects  *
''*   If you want multiple objects using this driver, you must     *
''*   copy the driver to a new file and make sure version long is  *
''*   unique in each version
''*   Added txflush                                                *
''*   Optimization perf                                            *
''*    1 port up to 750kbps                                        *
''*    2 port up to 230kbps                                        *
''*    3 port up to 140kbps                                        *
''*    4 port up to 100kbps                                        *
''*   Tested 4 ports to 115Kbps with 6MHz crystal                  *
''*   These are approx theoretical worse case you may get faster   *
''*   if port is active but idle                                   *
''*   Added RTS/CTS flow control                                   *
''*                                                                *
''*   There is no perf penalty supporting 4 serial ports when they *
''*   are not enabled                                              *
''*   There is no perf penalty supporting CTS and RTS              *
''*   Enabling CTS on any port costs 4 clocks per port             *
''*   Enabling RTS on any port costs 32 clocks per port            *
''*   Main Rx+Tx loop is ~256 clocks per port (without CTS/RTS)    *
''*   compared with FullDuplexSerial at ~356 clocks                *
''*                                                                *
''*   There is a cost to read/write a byte in the transmit/        *
''*   receive routines. The transmit cost is greater than the      *
''*   receive cost so between propellors you can run at max baud   *
''*   rate. If receiving from another device, the sending device   *
''*   needs a delay between each byte once you are above ~470kbps  *
''*   with 1 port enabled                                          *
''*                                                                *
''*   (TTA) I have not updated the following comments.             *
''*   Size:                                                        *
''*     Cog Initialzation code 1 x 8 + 4 x 25                      *
''*     Cog Receive code 4 x 30 words                              *
''*     Cog Transmit code 4 x 26 words                             *
''*     Spin/Cog circular buffer indexes 4 x 4 words               *
''*       Used in both spin and Cog and read/written in both       *
''*       directions                                               *
''*     Spin/Cog per port info 4 x 8 words                         *
''*       Passed from Spin to Cog on cog initialization            *
''*     Spin per port info 4 x 1 byte                              *
''*       Used by Spin                                             *
''*     Spin/Cog rx/tx buffer hub address 4 x 4 words              *
''*       Passed from Spin to Cog on cog initialization            *
''*     Spin/Cog rx/tx index hub address 4 x 4 words               *
''*       Passed from Spin to Cog on cog initialization            *
''*     Spin per port rx buffer 4 x 64 byte                        *
''*                                                                * 
''*     DWD Changed to 4 x 128 bytes                               *
''*       Read by Spin, written by cog                             *
''*                                                                * 
''*     Cog per port rx state 4 x 4 words (overlayed on rx buffer) *
''*       Used by Cog                                              *
''*     Spin per port tx buffer 4 x 16 byte                        *
''*       Written by Spin, read by Cog                             *
''*     Cog per port tx state 4 x 4 words (overlayed on tx buffer) *
''*       Used by Cog                                              *
''*     Cog constants 4 words                                      *
''*   A significant amount of space (4 x 16 words) is used for     *
''*   pre-calculated information: hub addresses, per port          *
''*   configuration. This speeds up the tx/rx routines at the cost *
''*   of this space.                                               *
''*                                                                *
''*   Note: There are 8 longs remaining in the cog's memory,       *
''*   expect to do some work to add features :).                   *
''*                                                                *
''*   DWD Note from Duane: Many of the longs in the cog image      *
''*   are only used in hub RAM.  There is still lots of room       *
''*   in cog RAM. (TTA) agreed, thanks DWD!                        *
''*                                                                *  
''*   7/1/08: Fixed bug of not receiving with only 1 port enabled  *
''*           Fixed bug of rts not working on ports 0, 2 and 3     *
''*  7/22/08: Missed a jmpret call in port 1 and 3 tx              *
''*           Fixed a bug in port 3 tx not increasing tx ptr       *
''*  7/24/08: Added version variable to change if need multiple    *
''*           copies of the driver                                 *
''*                                                                *
''******************************************************************

CON

  NOMODE                        = %000000
  INVERTRX                      = %000001
  INVERTTX                      = %000010
  OCTX                          = %000100
  NOECHO                        = %001000
  INVERTCTS                     = %010000
  INVERTRTS                     = %100000

  PINNOTUSED                    = -1                    'tx/tx/cts/rts pin is not used
  DEFAULTTHRESHOLD              = 0                     ' zero defaults to 3/4 of buffer length

  BAUD1200                      = 1200
  BAUD2400                      = 2400
  BAUD4800                      = 4800
  BAUD9600                      = 9600
  BAUD19200                     = 19200
  BAUD38400                     = 38400
  BAUD57600                     = 57600
  BAUD115200                    = 115200

' The following constants declare the sizes of the rx and tx buffers.
' Enter in the needed size in bytes for each rx and tx buffer
' These values can be any size within available memory. They do not have to be a power of two.
' Unused buffers can be reduced to 1 byte.
  RX_SIZE0                      = 16   ' receive buffer allocations
  RX_SIZE1                      = 1
  RX_SIZE2                      = 1
  RX_SIZE3                      = 1

  TX_SIZE0                      = 256  ' transmit buffer allocations
  TX_SIZE1                      = 1
  TX_SIZE2                      = 1
  TX_SIZE3                      = 1


  RXTX_BUFSIZE                  = (TX_SIZE0 + TX_SIZE1 + TX_SIZE2 + TX_SIZE3 + RX_SIZE0 + RX_SIZE1 + RX_SIZE2 + RX_SIZE3)
                                 ' total buffer footprint in bytes
                                 ' 77 longs, 308 bytes are available for buffers within the hub footprint of the object
                                 ' the final instruction in this program allocates additional buffer space beyond that if necessary
                                 ' to accomodate all of the buffers.
                                 ' if the sum totals to 308, then the buffers exactly fit within the object footprint.


PUB Init
''Always call init before adding ports
  Stop
  bytefill(@startfill, 0, (@endfill-@startfill))        ' initialize head/tails,port info and hub buffer pointers
  return @rxsize                                        ' TTA returns pointer to data structure, buffer sizes.

PUB AddPort(port,rxpin,txpin,ctspin,rtspin,rtsthreshold,mode,baudrate)
'' Call AddPort to define each port
'' port 0-3 port index of which serial port
'' rx/tx/cts/rtspin pin number                          XXX#PINNOTUSED if not used
'' rtsthreshold - buffer threshold before rts is used   XXX#DEFAULTTHRSHOLD means use default
'' mode bit 0 = invert rx                               XXX#INVERTRX
'' mode bit 1 = invert tx                               XXX#INVERTTX
'' mode bit 2 = open-drain/source tx                    XXX#OCTX
'' mode bit 3 = ignore tx echo on rx                    XXX#NOECHO
'' mode bit 4 = invert cts                              XXX#INVERTCTS
'' mode bit 5 = invert rts                              XXX#INVERTRTS
'' baudrate
  if cog OR (port > 3)
    abort
  if rxpin <> -1
    long[@rxmask][port] := |< rxpin
  if txpin <> -1
    long[@txmask][port] := |< txpin
  if ctspin <> -1
    long[@ctsmask][port] := |< ctspin
  if rtspin <> -1
    long[@rtsmask][port] := |< rtspin
    if (rtsthreshold > 0) AND (rtsthreshold < rxsize[port])           ' (TTA) modified for variable buffer size
      long[@rtssize][port] := rtsthreshold
    else
      long[@rtssize][port] := rxsize[port]*3/4                        'default rts threshold 3/4 of buffer  TTS ref RX_BUFSIZE
  long[@rxtx_mode][port] := mode
  if mode & INVERTRX
    byte[@rxchar][port] := $ff
  long[@bit_ticks][port] := (clkfreq / baudrate)
  long[@bit4_ticks][port] := long[@bit_ticks][port] >> 2

PUB Start : okay
'' Call start to start cog
'' Start serial driver - starts a cog
'' returns false if no cog available
''
'' tx buffers will start within the object footprint, overlaying certain locations that were initialized in spin
'' for  use within the cog but are not needed by spin thereafter and are not needed for object restart.

  txbuff_tail_ptr := txbuff_ptr  := @buffers                 ' (TTA) all buffers are calculated as offsets from this address.
  txbuff_tail_ptr1 := txbuff_ptr1 := txbuff_ptr + txsize      'base addresses of the corresponding port buffer.
  txbuff_tail_ptr2 := txbuff_ptr2 := txbuff_ptr1 + txsize1
  txbuff_tail_ptr3 := txbuff_ptr3 := txbuff_ptr2 + txsize2
  rxbuff_head_ptr := rxbuff_ptr  := txbuff_ptr3 + txsize3     ' rx buffers follow immediately after the tx buffers, by size
  rxbuff_head_ptr1 := rxbuff_ptr1 := rxbuff_ptr + rxsize
  rxbuff_head_ptr2 := rxbuff_ptr2 :=  rxbuff_ptr1 + rxsize1
  rxbuff_head_ptr3 := rxbuff_ptr3 :=  rxbuff_ptr2 + rxsize2
                                                        ' note that txbuff_ptr ... rxbuff_ptr3 are the base addresses fixed
                                                        ' in memory for use by both spin and pasm
                                                        ' while txbuff_tail_ptr ... rxbuff_head_ptr3 are dynamic addresses used only by pasm
                                                        ' and here initialized to point to the start of the buffers.
                                                             ' the rx buffer #3 comes last, up through address @endfill
  rx_head_ptr  := @rx_head                              ' (TTA) note: addresses of the head and tail counts are passed to the cog
  rx_head_ptr1 := @rx_head1                             ' if that is confusing, take heart.   These are pointers to pointers to pointers
  rx_head_ptr2 := @rx_head2
  rx_head_ptr3 := @rx_head3
  rx_tail_ptr  := @rx_tail
  rx_tail_ptr1 := @rx_tail1
  rx_tail_ptr2 := @rx_tail2
  rx_tail_ptr3 := @rx_tail3
  tx_head_ptr  := @tx_head
  tx_head_ptr1 := @tx_head1
  tx_head_ptr2 := @tx_head2
  tx_head_ptr3 := @tx_head3
  tx_tail_ptr  := @tx_tail
  tx_tail_ptr1 := @tx_tail1
  tx_tail_ptr2 := @tx_tail2
  tx_tail_ptr3 := @tx_tail3
  okay := cog := cognew(@entry, @rx_head) + 1

PUB Stop
'' Stop serial driver - frees a cog
  if cog
    cogstop(cog~ - 1)


PUB getCogID : result
  return cog -1

PUB rxflush(port)
'' Flush receive buffer, here until empty.
  repeat while rxcheck(port) => 0

PUB rxHowFull(port)    ' (TTA) added method
'' returns number of chars in rx buffer
  return ((rx_head[port] - rx_tail[port]) + rxsize[port]) // rxsize[port]
'   rx_head and rx_tail are values in the range 0=< ... < RX_BUFSIZE


PUB rxcheck(port) : rxbyte
'' Check if byte received (never waits)
'' returns -1 if no byte received, $00..$FF if byte
'' (TTA) simplified references
  if port > 3
    abort
  rxbyte--
  if rx_tail[port] <> rx_head[port]
    rxbyte := rxchar[port] ^ byte[rxbuff_ptr[port]+rx_tail[port]]
    rx_tail[port] := (rx_tail[port] + 1) // rxsize[port]

PUB rxtime(port,ms) : rxbyte | t
'' Wait ms milliseconds for a byte to be received
'' returns -1 if no byte received, $00..$FF if byte
  t := cnt
  repeat until (rxbyte := rxcheck(port)) => 0 or (cnt - t) / (clkfreq / 1000) > ms

PUB rx(port) : rxbyte
'' Receive byte (may wait for byte)
'' returns $00..$FF
  repeat while (rxbyte := rxcheck(port)) < 0

PUB tx(port,txbyte)
'' Send byte (may wait for room in buffer)
  if port > 3
    abort
  repeat until (tx_tail[port] <> (tx_head[port] + 1) // txsize[port])
  byte[txbuff_ptr[port]+tx_head[port]] := txbyte
  tx_head[port] := (tx_head[port] + 1) // txsize[port]

  if rxtx_mode[port] & NOECHO
    rx(port)

PUB txflush(port)
  repeat until (long[@tx_tail][port] == long[@tx_head][port])

PUB str(port,stringptr)
'' Send zstring
  strn(port,stringptr,strsize(stringptr))

PUB strn(port,stringptr,nchar)
'' Send counted string
  repeat nchar
    tx(port,byte[stringptr++])

DAT
'***********************************
'* Assembly language serial driver *
'***********************************
'
                        org 0
'
' Entry
'                   
'To maximize the speed of rx and tx processing, all the mode checks are no longer inline
'The initialization code checks the modes and modifies the rx/tx code for that mode
'e.g. the if condition for rx checking for a start bit will be inverted if mode INVERTRX
'is it, similar for other mode flags
'The code is also patched depending on whether a cts or rts pin are supplied. The normal
' routines support cts/rts processing. If the cts/rts mask is 0, then the code is patched
'to remove the addtional code. This means I/O modes and CTS/RTS handling adds no extra code
'in the rx/tx routines which not required.
'Similar with the co-routine variables. If a rx or tx pin is not configured the co-routine
'variable for the routine that handles that pin is modified so the routine is never called
'We start with port 3 and work down to ports because we will be updating the co-routine pointers
'and the order matters. e.g. we can update txcode3 and then update rxcode3 based on txcode3.
'(TTA): coroutine patch was not working in the way originally described.   (TTA) patched
'unused coroutines jmprets become simple jmps.
' Tim's comments about the order from 3 to 0 no longer apply.

' The following 8 locations are skipped at entry due to if_never.
' The mov instruction and the destination address are here only for syntax.
' the important thing are the source field
' primed to contain the start address of each port routine.
' When jmpret instructions are executed, the source adresses here are used for jumps
' And new source addresses will be written in the process.
entry                   
rxcode  if_never        mov     rxcode,#receive       ' set source fields to initial entry points
txcode  if_never        mov     txcode,#transmit
rxcode1 if_never        mov     rxcode1,#receive1
txcode1 if_never        mov     txcode1,#transmit1
rxcode2 if_never        mov     rxcode2,#receive2
txcode2 if_never        mov     txcode2,#transmit2
rxcode3 if_never        mov     rxcode3,#receive3
txcode3 if_never        mov     txcode3,#transmit3

' INITIALIZATIONS ==============================================================================
' port 3 initialization -------------------------------------------------------------
                        test    rxtx_mode3,#OCTX wz   'init tx pin according to mode
                        test    rxtx_mode3,#INVERTTX wc
        if_z_ne_c or            outa,txmask3
        if_z            or      dira,txmask3
                                                      'patch tx routine depending on invert and oc
                                                      'if invert change muxc to muxnc
                                                      'if oc change outa to dira
        if_z_eq_c or            txout3,domuxnc        'patch muxc to muxnc
        if_nz           movd    txout3,#dira          'change destination from outa to dira
                                                      'patch rx wait for start bit depending on invert
                        test    rxtx_mode3,#INVERTRX wz 'wait for start bit on rx pin
        if_nz           xor     start3,doifc2ifnc     'if_c jmp to if_nc
                                                      'patch tx routine depending on whether cts is used
                                                      'and if it is inverted
                        or      ctsmask3,#0     wz    'cts pin? z not set if in use
        if_nz           test    rxtx_mode3,#INVERTCTS wc 'c set if inverted
        if_nz_and_c     or      ctsi3,doif_z_or_nc    'if_nc jmp   (TTA) reversed order to correctly invert CTS
        if_nz_and_nc    or      ctsi3,doif_z_or_c     'if_c jmp
                                                      'if not cts remove the test by moving
                                                      'the transmit entry point down 1 instruction
                                                      'and moving the jmpret over the cts test
                                                      'and changing co-routine entry point
        if_z            mov     txcts3,transmit3      'copy the jmpret over the cts test
        if_z            movs    ctsi3,#txcts3         'patch the jmps to transmit to txcts0
        if_z            add     txcode3,#1            'change co-routine entry to skip first jmpret
                                                      'patch rx routine depending on whether rts is used
                                                      'and if it is inverted
                        or      rtsmask3,#0     wz
        if_nz           or      dira,rtsmask3          ' (TTA) rts needs to be an output
        if_nz           test    rxtx_mode3,#INVERTRTS wc
        if_nz_and_nc    or      rts3,domuxnc          'patch muxc to muxnc
        if_z            mov     norts3,rec3i          'patch rts code to a jmp #receive3
        if_z            movs    start3,#receive3      'skip all rts processing                  

                        or      txmask3,#0      wz       'if tx pin not used
        if_z            movi    transmit3, #%010111_000  ' patch it out entirely by making the jmpret into a jmp (TTA)
                        or      rxmask3,#0      wz       'ditto for rx routine
        if_z            movi    receive3, #%010111_000   ' (TTA)
                                                         ' in pcFullDuplexSerial4fc, the bypass was ostensibly done
                                                         ' by patching the co-routine variables,
                                                         ' but it was commented out, and didn't work when restored
                                                         ' so I did it by changing the affected jmpret to jmp.
                                                         ' Now the jitter is MUCH reduced.
' port 2 initialization -------------------------------------------------------------
                        test    rxtx_mode2,#OCTX wz   'init tx pin according to mode
                        test    rxtx_mode2,#INVERTTX wc
        if_z_ne_c       or      outa,txmask2
        if_z            or      dira,txmask2
        if_z_eq_c       or      txout2,domuxnc        'patch muxc to muxnc
        if_nz           movd    txout2,#dira          'change destination from outa to dira
                        test    rxtx_mode2,#INVERTRX wz 'wait for start bit on rx pin
        if_nz           xor     start2,doifc2ifnc     'if_c jmp to if_nc
                        or      ctsmask2,#0     wz
        if_nz           test    rxtx_mode2,#INVERTCTS wc
        if_nz_and_c     or      ctsi2,doif_z_or_nc    'if_nc jmp   (TTA) reversed order to correctly invert CTS
        if_nz_and_nc    or      ctsi2,doif_z_or_c     'if_c jmp
       if_z            mov     txcts2,transmit2      'copy the jmpret over the cts test
        if_z            movs    ctsi2,#txcts2         'patch the jmps to transmit to txcts0  
        if_z            add     txcode2,#1            'change co-routine entry to skip first jmpret
                        or      rtsmask2,#0     wz
        if_nz           or      dira,rtsmask2          ' (TTA) rts needs to be an output
        if_nz           test    rxtx_mode2,#INVERTRTS wc
        if_nz_and_nc    or      rts2,domuxnc          'patch muxc to muxnc
        if_z            mov     norts2,rec2i          'patch to a jmp #receive2
        if_z            movs    start2,#receive2      'skip all rts processing                  

                                or txmask2,#0    wz       'if tx pin not used
        if_z            movi    transmit2, #%010111_000   ' patch it out entirely by making the jmpret into a jmp (TTA)
                        or      rxmask2,#0      wz        'ditto for rx routine
        if_z            movi    receive2, #%010111_000    ' (TTA)

' port 1 initialization -------------------------------------------------------------
                        test    rxtx_mode1,#OCTX wz   'init tx pin according to mode
                        test    rxtx_mode1,#INVERTTX wc
        if_z_ne_c       or      outa,txmask1
        if_z            or      dira,txmask1
        if_z_eq_c       or      txout1,domuxnc        'patch muxc to muxnc
        if_nz           movd    txout1,#dira          'change destination from outa to dira
                        test    rxtx_mode1,#INVERTRX wz 'wait for start bit on rx pin
        if_nz           xor     start1,doifc2ifnc     'if_c jmp to if_nc
                        or      ctsmask1,#0     wz
        if_nz           test    rxtx_mode1,#INVERTCTS wc
        if_nz_and_c     or      ctsi1,doif_z_or_nc    'if_nc jmp   (TTA) reversed order to correctly invert CTS
        if_nz_and_nc    or      ctsi1,doif_z_or_c     'if_c jmp
        if_z            mov     txcts1,transmit1      'copy the jmpret over the cts test
        if_z            movs    ctsi1,#txcts1         'patch the jmps to transmit to txcts0  
        if_z            add     txcode1,#1            'change co-routine entry to skip first jmpret
                                                      'patch rx routine depending on whether rts is used
                                                      'and if it is inverted
                        or      rtsmask1,#0     wz
        if_nz           or      dira,rtsmask1          ' (TTA) rts needs to be an output
        if_nz           test    rxtx_mode1,#INVERTRTS wc
        if_nz_and_nc    or      rts1,domuxnc          'patch muxc to muxnc
        if_z            mov     norts1,rec1i          'patch to a jmp #receive1
        if_z            movs    start1,#receive1      'skip all rts processing                  

                        or      txmask1,#0      wz       'if tx pin not used
        if_z            movi    transmit1, #%010111_000  ' patch it out entirely by making the jmpret into a jmp (TTA)
                        or      rxmask1,#0      wz       'ditto for rx routine
        if_z            movi    receive1, #%010111_000   ' (TTA)

' port 0 initialization -------------------------------------------------------------
                        test    rxtx_mode,#OCTX wz    'init tx pin according to mode
                        test    rxtx_mode,#INVERTTX wc
        if_z_ne_c       or      outa,txmask
        if_z            or      dira,txmask
                                                      'patch tx routine depending on invert and oc
                                                      'if invert change muxc to muxnc
                                                      'if oc change out1 to dira
        if_z_eq_c       or      txout0,domuxnc        'patch muxc to muxnc
        if_nz           movd    txout0,#dira          'change destination from outa to dira
                                                      'patch rx wait for start bit depending on invert
                        test    rxtx_mode,#INVERTRX wz  'wait for start bit on rx pin
        if_nz           xor     start0,doifc2ifnc     'if_c jmp to if_nc
                                                      'patch tx routine depending on whether cts is used
                                                      'and if it is inverted
                        or      ctsmask,#0     wz     'cts pin? z not set if in use
        if_nz           or      dira,rtsmask          ' (TTA) rts needs to be an output
        if_nz           test    rxtx_mode,#INVERTCTS wc 'c set if inverted
        if_nz_and_c     or      ctsi0,doif_z_or_nc    'if_nc jmp   (TTA) reversed order to correctly invert CTS
        if_nz_and_nc    or      ctsi0,doif_z_or_c     'if_c jmp
        if_z            mov     txcts0,transmit       'copy the jmpret over the cts test
        if_z            movs    ctsi0,#txcts0         'patch the jmps to transmit to txcts0  
        if_z            add     txcode,#1             'change co-routine entry to skip first jmpret
                                                      'patch rx routine depending on whether rts is used
                                                      'and if it is inverted
                        or      rtsmask,#0     wz     'rts pin, z not set if in use
        if_nz           test    rxtx_mode,#INVERTRTS wc
        if_nz_and_nc    or      rts0,domuxnc          'patch muxc to muxnc
        if_z            mov     norts0,rec0i          'patch to a jmp #receive
        if_z            movs    start0,#receive       'skip all rts processing if not used

                        or      txmask,#0      wz       'if tx pin not used
        if_z            movi    transmit, #%010111_000  ' patch it out entirely by making the jmpret into a jmp (TTA)
                        or      rxmask,#0      wz       'ditto for rx routine
        if_z            movi    receive, #%010111_000   ' (TTA)
'
' MAIN LOOP  =======================================================================================
' Receive0 -------------------------------------------------------------------------------------
receive                 jmpret  rxcode,txcode         'run a chunk of transmit code, then return
                                                      'patched to a jmp if pin not used                        
                        test    rxmask,ina      wc
start0  if_c            jmp     #norts0               'go check rts if no start bit
                                                      ' have to check rts because other process may remove chars
                                                      'will be patched to jmp #receive if no rts  

                        mov     rxbits,#9             'ready to receive byte
                        mov     rxcnt,bit4_ticks      '1/4 bits
                        add     rxcnt,cnt                          

:bit                    add     rxcnt,bit_ticks       '1 bit period
                        
:wait                   jmpret  rxcode,txcode         'run a chuck of transmit code, then return

                        mov     t1,rxcnt              'check if bit receive period done
                        sub     t1,cnt
                        cmps    t1,#0           wc
        if_nc           jmp     #:wait

                        test    rxmask,ina      wc    'receive bit on rx pin
                        rcr     rxdata,#1
                        djnz    rxbits,#:bit          'get remaining bits
                        test    rxtx_mode,#INVERTRX  wz      'find out if rx is inverted
        if_z_ne_c       jmp     #receive              'abort if no stop bit   (TTA) (from serialMirror)
                        jmpret  rxcode,txcode         'run a chunk of transmit code, then return
                        
                        shr     rxdata,#32-9          'justify and trim received byte

                        wrbyte  rxdata,rxbuff_head_ptr'{7-22} '1wr
                        add     rx_head,#1
                        cmpsub  rx_head,rxsize   ' (TTA) allows non-binary buffer size
                        wrlong  rx_head,rx_head_ptr   '{8}     '2wr
                        mov     rxbuff_head_ptr,rxbuff_ptr 'calculate next byte head_ptr
                        add     rxbuff_head_ptr,rx_head
norts0                  rdlong  rx_tail,rx_tail_ptr   '{7-22 or 8} will be patched to jmp #r3 if no rts
                                                                '1rd
                        mov     t1,rx_head
                        sub     t1,rx_tail  wc          'calculate number bytes in buffer, (TTA) add wc
'                        and     t1,#$7F               'fix wrap
        if_c            add     t1,rxsize           ' fix wrap, (TTA) change
                        cmps    t1,rtssize      wc    'is it more than the threshold
rts0                    muxc    outa,rtsmask          'set rts correctly

rec0i                   jmp     #receive              'byte done, receive next byte
'
' Receive1 -------------------------------------------------------------------------------------
'
receive1                jmpret  rxcode1,txcode1       'run a chunk of transmit code, then return
                        
                        test    rxmask1,ina     wc
start1  if_c            jmp     #norts1               'go check rts if no start bit

                        mov     rxbits1,#9            'ready to receive byte
                        mov     rxcnt1,bit4_ticks1    '1/4 bits
                        add     rxcnt1,cnt                          

:bit1                   add     rxcnt1,bit_ticks1     '1 bit period
                        
:wait1                  jmpret  rxcode1,txcode1       'run a chuck of transmit code, then return

                        mov     t1,rxcnt1             'check if bit receive period done
                        sub     t1,cnt
                        cmps    t1,#0           wc
        if_nc           jmp     #:wait1

                        test    rxmask1,ina     wc    'receive bit on rx pin
                        rcr     rxdata1,#1
                        djnz    rxbits1,#:bit1

                        test    rxtx_mode1,#INVERTRX  wz      'find out if rx is inverted
        if_z_ne_c       jmp     #receive1              'abort if no stop bit   (TTA) (from serialMirror)

                        jmpret  rxcode1,txcode1       'run a chunk of transmit code, then return
                        shr     rxdata1,#32-9         'justify and trim received byte

                        wrbyte  rxdata1,rxbuff_head_ptr1 '7-22
                        add     rx_head1,#1
                        cmpsub  rx_head1,rxsize1         ' (TTA) allows non-binary buffer size
                        wrlong  rx_head1,rx_head_ptr1
                        mov     rxbuff_head_ptr1,rxbuff_ptr1 'calculate next byte head_ptr
                        add     rxbuff_head_ptr1,rx_head1
norts1                  rdlong  rx_tail1,rx_tail_ptr1    '7-22 or 8 will be patched to jmp #r3 if no rts
                        mov     t1,rx_head1
                        sub     t1,rx_tail1    wc
        if_c            add     t1,rxsize1           ' fix wrap, (TTA) change
                        cmps    t1,rtssize1     wc
rts1                    muxc    outa,rtsmask1

rec1i                   jmp     #receive1             'byte done, receive next byte
'
' Receive2 -------------------------------------------------------------------------------------
'
receive2                jmpret  rxcode2,txcode2       'run a chunk of transmit code, then return
                        
                        test    rxmask2,ina     wc
start2 if_c             jmp     #norts2               'go check rts if no start bit
        
                        mov     rxbits2,#9            'ready to receive byte
                        mov     rxcnt2,bit4_ticks2    '1/4 bits
                        add     rxcnt2,cnt                          

:bit2                   add     rxcnt2,bit_ticks2     '1 bit period
                        
:wait2                  jmpret  rxcode2,txcode2       'run a chuck of transmit code, then return

                        mov     t1,rxcnt2             'check if bit receive period done
                        sub     t1,cnt
                        cmps    t1,#0           wc
        if_nc           jmp     #:wait2

                        test    rxmask2,ina     wc    'receive bit on rx pin
                        rcr     rxdata2,#1
                        djnz    rxbits2,#:bit2
                        test    rxtx_mode2,#INVERTRX  wz      'find out if rx is inverted
        if_z_ne_c       jmp     #receive2              'abort if no stop bit   (TTA) (from serialMirror)

                        jmpret  rxcode2,txcode2       'run a chunk of transmit code, then return
                        shr     rxdata2,#32-9         'justify and trim received byte

                        wrbyte  rxdata2,rxbuff_head_ptr2 '7-22
                        add     rx_head2,#1
                        cmpsub  rx_head2,rxsize2        '  ' (TTA) allows non-binary buffer size
                        wrlong  rx_head2,rx_head_ptr2
                        mov     rxbuff_head_ptr2,rxbuff_ptr2 'calculate next byte head_ptr
                        add     rxbuff_head_ptr2,rx_head2
norts2                  rdlong  rx_tail2,rx_tail_ptr2    '7-22 or 8 will be patched to jmp #r3 if no rts
                        mov     t1,rx_head2
                        sub     t1,rx_tail2    wc
        if_c            add     t1,rxsize2            ' fix wrap, (TTA) change
                        cmps    t1,rtssize2     wc
rts2                    muxc    outa,rtsmask2

rec2i                   jmp     #receive2             'byte done, receive next byte
'
' Receive3 -------------------------------------------------------------------------------------
'
receive3                jmpret  rxcode3,txcode3       'run a chunk of transmit code, then return

                        test    rxmask3,ina     wc
start3 if_c             jmp     #norts3               'go check rts if no start bit

                        mov     rxbits3,#9            'ready to receive byte
                        mov     rxcnt3,bit4_ticks3    '1/4 bits
                        add     rxcnt3,cnt                          

:bit3                   add     rxcnt3,bit_ticks3     '1 bit period
                        
:wait3                  jmpret  rxcode3,txcode3       'run a chuck of transmit code, then return

                        mov     t1,rxcnt3             'check if bit receive period done
                        sub     t1,cnt
                        cmps    t1,#0           wc
        if_nc           jmp     #:wait3

                        test    rxmask3,ina     wc    'receive bit on rx pin
                        rcr     rxdata3,#1
                        djnz    rxbits3,#:bit3
                        test    rxtx_mode3,#INVERTRX  wz      'find out if rx is inverted
        if_z_ne_c       jmp     #receive3              'abort if no stop bit   (TTA) (from serialMirror)

                        jmpret  rxcode3,txcode3       'run a chunk of transmit code, then return
                        shr     rxdata3,#32-9         'justify and trim received byte

                        wrbyte  rxdata3,rxbuff_head_ptr3 '7-22
                        add     rx_head3,#1
                        cmpsub  rx_head3,rxsize3         ' (TTA) allows non-binary buffer size
                        wrlong  rx_head3,rx_head_ptr3    '8
                        mov     rxbuff_head_ptr3,rxbuff_ptr3 'calculate next byte head_ptr
                        add     rxbuff_head_ptr3,rx_head3
norts3                  rdlong  rx_tail3,rx_tail_ptr3    '7-22 or 8, may be patched to jmp #r3 if no rts
                        mov     t1,rx_head3
                        sub     t1,rx_tail3    wc
        if_c            add     t1,rxsize3            ' fix wrap, (TTA) change
                        cmps    t1,rtssize3     wc    'is buffer more that 3/4 full?
rts3                    muxc    outa,rtsmask3

rec3i                   jmp     #receive3             'byte done, receive next byte
'
' TRANSMIT =======================================================================================
'
transmit                jmpret  txcode,rxcode1        'run a chunk of receive code, then return
                                                      'patched to a jmp if pin not used                        
                        
txcts0                  test    ctsmask,ina     wc    'if flow-controlled dont send
                        rdlong  t1,tx_head_ptr        '{7-22} - head[0]
                        cmp     t1,tx_tail      wz    'tail[0]
ctsi0   if_z            jmp     #transmit             'may be patched to if_z_or_c or if_z_or_nc

                        rdbyte  txdata,txbuff_tail_ptr '{8}
                        add     tx_tail,#1
                        cmpsub     tx_tail,txsize    wz   ' (TTA) for individually sized buffers, will zero at rollover
                        wrlong  tx_tail,tx_tail_ptr    '{8}  
        if_z            mov     txbuff_tail_ptr,txbuff_ptr 'reset tail_ptr if we wrapped
        if_nz           add     txbuff_tail_ptr,#1    'otherwise add 1
                        
                        jmpret  txcode,rxcode1

                        shl     txdata,#2
                        or      txdata,txbitor        'ready byte to transmit
                        mov     txbits,#11
                        mov     txcnt,cnt

txbit                   shr     txdata,#1       wc
txout0                  muxc    outa,txmask           'maybe patched to muxnc dira,txmask
                        add     txcnt,bit_ticks       'ready next cnt

:wait                   jmpret  txcode,rxcode1        'run a chunk of receive code, then return

                        mov     t1,txcnt              'check if bit transmit period done
                        sub     t1,cnt
                        cmps    t1,#0           wc
        if_nc           jmp     #:wait

                        djnz    txbits,#txbit         'another bit to transmit?
txjmp0                  jmp     ctsi0                 'byte done, transmit next byte
'
' Transmit1 -------------------------------------------------------------------------------------
'
transmit1               jmpret  txcode1,rxcode2       'run a chunk of receive code, then return
                        
txcts1                  test    ctsmask1,ina    wc    'if flow-controlled dont send
                        rdlong  t1,tx_head_ptr1
                        cmp     t1,tx_tail1     wz
ctsi1   if_z            jmp     #transmit1            'may be patched to if_z_or_c or if_z_or_nc

                        rdbyte  txdata1,txbuff_tail_ptr1
                        add     tx_tail1,#1
                        cmpsub     tx_tail1,txsize1   wz   ' (TTA) for individually sized buffers, will zero at rollover
                        wrlong  tx_tail1,tx_tail_ptr1
        if_z            mov     txbuff_tail_ptr1,txbuff_ptr1 'reset tail_ptr if we wrapped
        if_nz           add     txbuff_tail_ptr1,#1   'otherwise add 1

                        jmpret  txcode1,rxcode2       'run a chunk of receive code, then return
                        
                        shl     txdata1,#2
                        or      txdata1,txbitor       'ready byte to transmit
                        mov     txbits1,#11
                        mov     txcnt1,cnt

txbit1                  shr     txdata1,#1      wc
txout1                  muxc    outa,txmask1          'maybe patched to muxnc dira,txmask
                        add     txcnt1,bit_ticks1     'ready next cnt

:wait1                  jmpret  txcode1,rxcode2       'run a chunk of receive code, then return

                        mov     t1,txcnt1             'check if bit transmit period done
                        sub     t1,cnt
                        cmps    t1,#0           wc
        if_nc           jmp     #:wait1

                        djnz    txbits1,#txbit1       'another bit to transmit?
txjmp1                  jmp     ctsi1                 'byte done, transmit next byte
'
' Transmit2 -------------------------------------------------------------------------------------
'
transmit2               jmpret  txcode2,rxcode3       'run a chunk of receive code, then return
                        
txcts2                  test    ctsmask2,ina    wc    'if flow-controlled dont send
                        rdlong  t1,tx_head_ptr2
                        cmp     t1,tx_tail2     wz
ctsi2   if_z            jmp     #transmit2            'may be patched to if_z_or_c or if_z_or_nc

                        rdbyte  txdata2,txbuff_tail_ptr2
                        add     tx_tail2,#1
                        cmpsub     tx_tail2,txsize2   wz   ' (TTA) for individually sized buffers, will zero at rollover
                        wrlong  tx_tail2,tx_tail_ptr2
        if_z            mov     txbuff_tail_ptr2,txbuff_ptr2 'reset tail_ptr if we wrapped
        if_nz           add     txbuff_tail_ptr2,#1   'otherwise add 1

                        jmpret  txcode2,rxcode3

                        shl     txdata2,#2
                        or      txdata2,txbitor       'ready byte to transmit
                        mov     txbits2,#11
                        mov     txcnt2,cnt

txbit2                  shr     txdata2,#1      wc
txout2                  muxc    outa,txmask2          'maybe patched to muxnc dira,txmask
                        add     txcnt2,bit_ticks2     'ready next cnt

:wait2                  jmpret  txcode2,rxcode3       'run a chunk of receive code, then return

                        mov     t1,txcnt2             'check if bit transmit period done
                        sub     t1,cnt
                        cmps    t1,#0           wc
        if_nc           jmp     #:wait2

                        djnz    txbits2,#txbit2       'another bit to transmit?
txjmp2                  jmp     ctsi2                 'byte done, transmit next byte
'
' Transmit3 -------------------------------------------------------------------------------------
'
transmit3               jmpret  txcode3,rxcode        'run a chunk of receive code, then return
                        
txcts3                  test    ctsmask3,ina    wc    'if flow-controlled dont send
                        rdlong  t1,tx_head_ptr3
                        cmp     t1,tx_tail3     wz
ctsi3   if_z            jmp     #transmit3            'may be patched to if_z_or_c or if_z_or_nc

                        rdbyte  txdata3,txbuff_tail_ptr3
                        add     tx_tail3,#1
                        cmpsub     tx_tail3,txsize3   wz   ' (TTA) for individually sized buffers, will zero at rollover
                        wrlong  tx_tail3,tx_tail_ptr3
        if_z            mov     txbuff_tail_ptr3,txbuff_ptr3 'reset tail_ptr if we wrapped
        if_nz           add     txbuff_tail_ptr3,#1   'otherwise add 1

                        jmpret  txcode3,rxcode

                        shl     txdata3,#2
                        or      txdata3,txbitor       'ready byte to transmit
                        mov     txbits3,#11
                        mov     txcnt3,cnt

txbit3                  shr     txdata3,#1      wc
txout3                  muxc    outa,txmask3          'maybe patched to muxnc dira,txmask
                        add     txcnt3,bit_ticks3     'ready next cnt

:wait3                  jmpret  txcode3,rxcode        'run a chunk of receive code, then return

                        mov     t1,txcnt3             'check if bit transmit period done
                        sub     t1,cnt
                        cmps    t1,#0           wc
        if_nc           jmp     #:wait3

                        djnz    txbits3,#txbit3       'another bit to transmit?
txjmp3                  jmp     ctsi3                 'byte done, transmit next byte
'
'The following are constants used by pasm for patching the code, depending on options required
doifc2ifnc              long      $003c0000           'patch condition if_c to if_nc using xor
doif_z_or_c             long      $00380000           'patch condition if_z to if_z_or_c using or
doif_z_or_nc            long      $002c0000           'patch condition if_z to if_z_or_nc using or
domuxnc                 long      $04000000           'patch muxc to muxnc using or
txbitor                 long      $0401               'bits to or for transmitting, adding start and stop bits

' Buffer sizes initialized from CONstants and used by both spin and pasm

rxsize                  long      RX_SIZE0                  ' (TTA) size of the rx and tx buffers is available to pasm and spin
rxsize1                 long      RX_SIZE1                  ' these values are transfered from the declared CONstants
rxsize2                 long      RX_SIZE2                  ' at startup, individually configurable
rxsize3                 long      RX_SIZE3
txsize                  long      TX_SIZE0
txsize1                 long      TX_SIZE1
txsize2                 long      TX_SIZE2
txsize3                 long      TX_SIZE3


' Object memory from here to the end is zeroed in the init/stop method ---------------'
' Some locations within the next set of values, after being initialized to zero, are then filled with alternative options
' That are accessed from both spin and pasm
' Dont Change the order of these initialized variables within port groups of 4 without modifying
' the code to match - both spin and assembler

startfill
rxchar                byte      0             ' used by spin rxcheck, for inversion of received data
rxchar1               byte      0
rxchar2               byte      0
rxchar3               byte      0
cog                   long      0                   'cog flag/id
rxtx_mode             long      0             ' mode setting from values passed in by addport
rxtx_mode1            long      0             '
rxtx_mode2            long      0
rxtx_mode3            long      0
rx_head               long      0             ' rx head pointer, from 0 to size of rx buffer, used in spin and pasm
rx_head1              long      0             ' data is enqueued to this offset above base, rxbuff_ptr
rx_head2              long      0
rx_head3              long      0
rx_tail               long      0             ' rx tail pointer, ditto, zero to size of rx buffer
rx_tail1              long      0             ' data is dequeued from this offset above base, rxbuff_ptr
rx_tail2              long      0
rx_tail3              long      0
tx_head               long      0             ' tx head pointer, , from 0 to size of tx buffer, used in spin and pasm
tx_head1              long      0             ' data is enqueued to this offset above base, txbuff_ptr
tx_head2              long      0
tx_head3              long      0
tx_tail               long      0             ' tx tail pointer, ditto, zero to size of rx buffer
tx_tail1              long      0             ' data is transmitted from this offset above base, txbuff_ptr
tx_tail2              long      0
tx_tail3              long      0
rxbuff_ptr            long      0             ' These are the base hub addresses of the receive buffers
rxbuff_ptr1           long      0             ' initialized in spin, referenced in pasm and spin
rxbuff_ptr2           long      0             ' these buffers and sizes are individually configurable
rxbuff_ptr3           long      0
txbuff_ptr            long      0             ' These are the base hub addresses of the transmit buffers
txbuff_ptr1           long      0
txbuff_ptr2           long      0
txbuff_ptr3           long      0

'  Start of HUB overlay ------------------------------------------------------------------------
' Some locations within the next set of values, after being init'd to zero, are then filled from spin with options
' That are transferred to and accessed by the pasm cog once started, but no longer needed in spin.
' Therefore, tx and rx buffers start here and overlays the hub footprint of these variables.
' tx_buffers come first, 0,1,2,3, then rx buffers 0,1,2,3 by offset from "buffers"
overlay
buffers
txdata                long      0
txbits                long      0
txcnt                 long      0
txdata1               long      0
txbits1               long      0
txcnt1                long      0
txdata2               long      0
txbits2               long      0
txcnt2                long      0
txdata3               long      0
txbits3               long      0
txcnt3                long      0
rxdata                long      0
rxbits                long      0
rxcnt                 long      0
rxdata1               long      0
rxbits1               long      0
rxcnt1                long      0
rxdata2               long      0
rxbits2               long      0
rxcnt2                long      0
rxdata3               long      0
rxbits3               long      0
rxcnt3                long      0
t1                    long      0               ' this is a temporary variable used by pasm
rxmask                long      0               ' a single bit set, a mask for the pin used for receive, zero if port not used for receive
rxmask1               long      0
rxmask2               long      0
rxmask3               long      0
txmask                long      0               ' a single bit set, a mask for the pin used for transmit, zero if port not used for transmit
txmask1               long      0
txmask2               long      0
txmask3               long      0
ctsmask               long      0             ' a single bit set, a mask for the pin used for cts input, zero if port not using cts
ctsmask1              long      0
ctsmask2              long      0
ctsmask3              long      0
rtsmask               long      0             ' a single bit set, a mask for the pin used for rts output, zero if port not using rts
rtsmask1              long      0
rtsmask2              long      0
rtsmask3              long      0
bit4_ticks            long      0             ' bit ticks for start bit, 1/4 of standard bit
bit4_ticks1           long      0
bit4_ticks2           long      0
bit4_ticks3           long      0
bit_ticks             long      0             ' clock ticks per bit
bit_ticks1            long      0
bit_ticks2            long      0
bit_ticks3            long      0
rtssize               long      0             ' threshold in count of bytes above which will assert rts to stop flow
rtssize1              long      0
rtssize2              long      0
rtssize3              long      0
rxbuff_head_ptr         long      0             ' Hub address of data received, base plus offset
rxbuff_head_ptr1        long      0             ' pasm writes WRBYTE to hub at this address, initialized in spin to base address
rxbuff_head_ptr2        long      0
rxbuff_head_ptr3        long      0
txbuff_tail_ptr         long      0             ' Hub address of data tranmitted, base plus offset
txbuff_tail_ptr1        long      0             ' pasm reads RDBYTE from hub at this address, initialized in spin to base address
txbuff_tail_ptr2        long      0
txbuff_tail_ptr3        long      0
rx_head_ptr             long      0             ' pointer to the hub address of where the head and tail offset pointers are stored
rx_head_ptr1            long      0             ' these pointers are initialized in spin but then used only by pasm
rx_head_ptr2            long      0             ' the pasm cog has to know where in the hub to find those offsets.
rx_head_ptr3            long      0
rx_tail_ptr             long      0
rx_tail_ptr1            long      0
rx_tail_ptr2            long      0
rx_tail_ptr3            long      0
tx_head_ptr             long      0
tx_head_ptr1            long      0
tx_head_ptr2            long      0
tx_head_ptr3            long      0
tx_tail_ptr             long      0
tx_tail_ptr1            long      0
tx_tail_ptr2            long      0
tx_tail_ptr3            long      0
       '' ----------- End of the  object memory zeroed from startfill to endfill in the init/stop method ------
endfill
      FIT
'' The above is all of the necessary code that must fit in the cog
'' The following are extra bytes if necessary to provide the required rx and tx buffers.
'' the number required is computed from the aggregate buffer size declared, minus the above initialized but recycled variables.

extra                   byte    0 [RXTX_BUFSIZE - (RXTX_BUFSIZE <# (@extra - @overlay))]

{{
┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                                                   TERMS OF USE: MIT License                                                  │                                                            
├──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    │ 
│files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,    │
│modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software│
│is furnished to do so, subject to the following conditions:                                                                   │
│                                                                                                                              │
│The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.│
│                                                                                                                              │
│THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          │
│WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         │
│COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   │
│ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
}}
