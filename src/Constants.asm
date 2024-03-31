;-------------------------------------------------------------------------
; Abductor - Constants.asm
;-------------------------------------------------------------------------

IRQ1V                       = $204
IRQ2V                       = $206
EVENTV                      = $220

crtcAddr                    = $FE00 ; 6845 address
crtcData                    = $FE01 ; 6845 data
ulaMode                     = $FE20 ; ula video mode (Control)
ulaPalette                  = $FE21 ; ula colour palette
nuLACtrl                    = $FE22 ; (Control)
nulaPalette                 = $FE23 ; nuLA colour palette

sysVIAPortB                 = $FE40 ; sysVIA port B data
sysVIADataDirectionRegB     = $FE42 ; sysVIA port B io control
sysVIADataDirectionRegA     = $FE43 ; sysVIA Port A io control
sysVIATimer1CounterLow      = $FE44 ; sysVIA timer 1 low counter
sysVIATimer1CounterHigh     = $FE45 ; sysVIA timer 1 high counter
sysVIATimer1LatchLow        = $FE46 ; sysVIA timer 1 low latch
sysVIATimer1LatchHigh       = $FE47 ; sysVIA timer 1 high latch
sysVIATimer2CounterLow      = $FE48 ; sysVIA timer 2 counter low
sysVIATimer2CounterHigh     = $FE49 ; sysVIA timer 2 counter high
sysVIAAuxControlReg         = $FE4B ; sysVIA auxiliary control register  
sysVIAInterruptFlagReg      = $FE4D ; sysVIA interrupt flags  
sysVIAInterruptEnableReg    = $FE4E ; sysVIA interrupt enable    
sysVIAPortA                 = $FE4F ; sysVIA Port A data (no handshake)

OSRDCH              = $FFE0
OSASCI              = $FFE3
OSWRCH              = $FFEE
OSWORD              = $FFF1
OSBYTE              = $FFF4
OSCLI               = $FFF7
RESET_BEEB          = $FFFC

irq_A_save          = $FC

keyLeft             = $61
keyRight            = $42
keyReturn           = $49

keyUp               = $48
keyDown             = $68

KEYPRESS_NOTHING    = %00000000
KEYPRESS_UP         = %00000001
KEYPRESS_DOWN       = %00000010
KEYPRESS_LEFT       = %00000100
KEYPRESS_RIGHT      = %00001000
KEYPRESS_FIRE       = %00010000

SCREEN_WIDTH        = $28
ROW_OF_MEN          = $16 ; row 22
SCREEN_ADDRESS      = $7C08 ; $7C00
HIGHSCORE_ADDRESS   = SCREEN_ADDRESS + $11
CLEARSCREEN_ADDRESS = SCREEN_ADDRESS + $16
COPYRIGHT_ADDRESS   = SCREEN_ADDRESS + $55
DOTTED_LINE_ADDRESS = SCREEN_ADDRESS + $347

PLAYER_XPOS         = $0A
PLAYER_YPOS         = $14

BLACK               = $00
WHITE               = $01
RED                 = $02
CYAN                = $03
MAGENTA             = $04
GREEN               = $05
BLUE                = $06
YELLOW              = $07

; Characters used for Beebs Mode7
BLANK               = $20 ; 
HUMANOID            = $21 ; !
MISSILE             = $22 ; "
DOTTED_LINE         = $2D ; -
SKULL               = $4F ; O

SINGLE_SHIP         = $2B ; +
SINGLE_SHIPH1       = $3C ; <
SINGLE_SHIPH2       = $3E ; <

DOUBLE_SHIP1        = $41 ; A
DOUBLE_SHIP2        = $42 ; B
DOUBLE_SHIP3        = $43 ; C

DOUBLE_SHIPH1       = $44 ; D
DOUBLE_SHIPH2       = $45 ; E
DOUBLE_SHIPH3       = $46 ; F
DOUBLE_SHIPH4       = $47 ; G
