; Final version of LCD alarm clock program
; The following was written by Dr. Jesus Calvino-Fraga for ELEC 291:
; - Some of the pre-processor directives
; - Interrupt service initiations & routines (excluding alarm logic in Timer0_ISR and clock updates in Timer2_ISR)
; - The macros included in LCD_4bit.inc for writing to the LCD
; The rest is my original code, using his sample program as a template

; To include a file, wrap it with $NOLIST filename $LIST to prevent it from showing up in compiled .lst file
$NOLIST
$MODLP51 ; File containing register definitions for AT89LP51RC2
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload special function registers (SFRs), so:
TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

; EQU (equals): Define symbolic constants in code
CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

; Hardware wiring for interfacing with the clock
ALARM1_SWITCH equ P2.6
ALARM2_SWITCH equ P2.5
ALARM_SWITCH  equ P2.3
PAUSE_SWITCH  equ P2.0

DAY_BUTTON  equ P2.1
HOUR_BUTTON equ P2.4
MIN_BUTTON  equ P2.7
SEC_BUTTON  equ P4.4

ALARM_BUTTON equ P0.4

SOUND_OUT equ P3.7

; START ADDRESS
org 0x0000 ; Reset vector. 8051 executes instructions starting from this address
    ljmp main

; INTERRUPT SERVICE ROUTINE VECTORS
org 0x0003 ; External interrupt 0 vector (not used in this code)
	reti

org 0x000B ; Timer/Counter 0 overflow interrupt vector
	ljmp Timer0_ISR

org 0x0013 ; External interrupt 1 vector (not used in this code)
	reti

org 0x001B ; Timer/Counter 1 overflow interrupt vector (not used in this code)
	reti

org 0x0023 ; Serial port receive/transmit interrupt vector (not used in this code)
	reti
	
org 0x002B ; Timer/Counter 2 overflow interrupt vector
	ljmp Timer2_ISR

; Helpful referrence for the following code:
; http://www.keil.com/support/man/docs/a51/a51_cs_reference.htm

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30   ; Select data segment at address 0x30

Count1ms: ds 2 ; Allocate 2 bytes to a variable used to determine when half second has passed (0x30 to 0x40 reserved for Count1ms)

Seconds:  ds 1 ; The Seconds counter incremented in the ISR and displayed in the main loop (0x40 to 0x48 reserved for Seconds)
Minutes:  ds 1
Hours:    ds 1
Days:     ds 1

Seconds_Alarm1: ds 1
Minutes_Alarm1: ds 1
Hours_Alarm1:   ds 1
Days_Alarm1:    ds 1

Seconds_Alarm2: ds 1
Minutes_Alarm2: ds 1
Hours_Alarm2:   ds 1
Days_Alarm2:    ds 1

; In the 8051 we have variables that are 1-bit in size.  
; We can use the setb, clr, jb, and jnb instructions with these variables.
; This is how you define a 1-bit variable:
bseg ; (Select bit segment at in bit-addressible part of memory in range 20.0H-2F.7H)

seconds_flag: dbit 1 ; Set to one in the ISR every time 1000 ms had passed (reserve one bit for seconds_flag)
alarm_flag:   dbit 1

cseg ; Select code segment in 0x0000-0xFFFF
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                  123456789ABCDEFG    <- This helps determine the location of the counter
Clock:         db 'xxx xx:xx:xx    ', 0
Weekday_Alarm: db '1   MON 00:00:00', 0
Weekend_Alarm: db '2   SAT 00:00:00', 0
Empty_Line:    db '                ', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD  ; TMOD = Timer mode register
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-bit timer (input from internal system clock)
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	
	; Set autoreload value
	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
	
	; Enable the timer and interrupts
	setb ET0  ; Enable timer 0 interrupt
	setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	push acc
	push psw

	mov a, alarm_flag
	cjne a, #0x01, Skip_Sound_Out
	cpl SOUND_OUT ; Connect speaker to P3.7!

Skip_Sound_Out:
	pop psw
	pop acc
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
	setb ET2  ; Enable timer 2 interrupt
	setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1	  ; This instruction executes when low 8 bits overflow/roll over (and skipped otherwise)

Inc_Done:
	; Check if second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb seconds_flag ; Let the main program know half second had passed
	cpl TR0 			   ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a

    	; Increment the seconds counter
	mov a, Seconds
	add a, #0x01
	da a
	cjne a, #0x60, Update_Seconds ; 0x60 is 60 BCD

	; Increment the minutes counter
	mov a, Minutes
	add a, #0x01
	da a
	cjne a, #0x60, Update_Minutes
	
	; Increment the hours counter
	mov a, Hours
	add a, #0x01
	da a
	cjne a, #0x24, Update_Hours

	; Increment the days counter
	mov a, Days
	add a, #0x01
	da a
	cjne a, #0x7, Update_Days
	clr a

Update_Days:
	mov Days, a
	clr a

Update_Hours:
	mov Hours, a
	clr a

Update_Minutes:
	mov Minutes, a
	clr a

Update_Seconds:
	mov Seconds, a
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
	mov SP, #0x7F
	lcall Timer0_Init ; Initialize timer 0 as timer for speaker
	lcall Timer2_Init ; Initialize timer 2 as counter for LCD
	
	; Configure the ports in bidirectional mode
	; (i.e. can be used both as an input and output without the need to reconfigure the port):
	mov P0M0, #0
	mov P0M1, #0

	mov P2M0, #0
	mov P2M1, #0
    
	setb EA ; Enable Global interrupts
    
	lcall LCD_4BIT ; Configure LCD screen
    
	; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
	Send_Constant_String(#Clock)

	setb seconds_flag ; Reminder: a bit in memory set to one in the ISR every time 1000 ms has passed
	mov a, #0x00
	mov alarm_flag, a

	; Initialize clock variables
	mov Seconds, #0x00
	mov Minutes, #0x00
	mov Hours, #0x00
	mov Days, #0x00

	; Initialize weekday alarm variables
	mov Seconds_Alarm1, #0x00
	mov Minutes_Alarm1, #0x00
	mov Hours_Alarm1, #0x00
	mov Days_Alarm1, #0x00

	; Initialize weekend alarm variables
	mov Seconds_Alarm2, #0x00
	mov Minutes_Alarm2, #0x00
	mov Hours_Alarm2, #0x00
	mov Days_Alarm2, #0x05 ; Start Days on Saturday
	
; After initialization the program stays in this 'forever' loop
forever:
	;Check if an alarm has triggered
	ljmp Check_Alarm1

After_Check_Alarm:
	; Check if user wants to pause/adjust the clock
	jb PAUSE_SWITCH, Skip_Check_Pause ; If switch is off (Vcc), skip going into pause mode
	clr TR2                           ; Pause the timer before entering. Do it here because we only want this done once
	ljmp Check_Pause                  ; Used because jb/jnb instructions don't support ljmp

Skip_Check_Pause:
	; Check if user wants to adjust the alarms
	jb ALARM_SWITCH, idle

	; Reach here if user wants to enter alarm mode. Printing initial alarm state here since we only want this done once
	lcall Display_Alarm
	ljmp Check_Alarm

; "Idle" branch - Arrive here from loop b/c reset hasn't been pressed. Proceed to print only when seconds_flag high. Most time spent bouncing btwn here & forever
idle:
	jnb seconds_flag, forever
	
; "Print new number" branch - when ISR sets seconds bit
print:
    clr seconds_flag     ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	
	Set_Cursor(1, 11)    ; the place in the LCD where we want the Seconds counter value
	Display_BCD(Seconds) ; This macro is also in 'LCD_4bit.inc'

	Set_Cursor(1, 8)
	Display_BCD(Minutes)

	Set_Cursor(1, 5)
	Display_BCD(Hours)

	Set_Cursor(1, 1)
	Display_day(Days)
	
    ljmp forever

;---------------;
; PAUSE MODE    ;
;---------------;
Check_Pause:
	; BUG: The following instruction should jump to Check_Seconds instead, but this enables a bug where:
	; 1. The seconds counter begins incrementing until overflow
	; 2. The seconds counter begins incrementing when one of the other time variables overflows
	jnb PAUSE_SWITCH, Check_Minutes ; As long as the switch is on (grounded), check for time changes

	setb TR2   ; Re-set the timer before returning
	ljmp print ; Exit pause

;--------------------------------------------------------------------------------
Check_Seconds: 
	jb SEC_BUTTON, Check_Minutes ; If button not pressed, move on to check minutes
	Wait_Milli_Seconds(#125)
	jb SEC_BUTTON, Check_Minutes
	ljmp Increment_Seconds ; Else, adjust seconds

Check_Minutes:
	jb MIN_BUTTON, Check_Hours
	Wait_Milli_Seconds(#125)
	jb MIN_BUTTON, Check_Hours
	ljmp Increment_Minutes

Check_Hours:
	jb HOUR_BUTTON, Check_Days
	Wait_Milli_Seconds(#125)
	jb HOUR_BUTTON, Check_Days
	ljmp Increment_Hours

Check_Days:
	jb DAY_BUTTON, Check_Pause
	Wait_Milli_Seconds(#125)
	jb DAY_BUTTON, Check_Pause
;--------------------------------------------------------------------------------
Increment_Days:
	mov a, Days
	add a, #0x01
	da a
	cjne a, #0x07, Print_Days
	clr a ; Days is cleared only if it overflows to BCD 7

Print_Days:
	mov Days, a
	Set_Cursor(1,1)
	Display_day(Days)
	ljmp Check_Pause
;--------------------------------------------------------------------------------
Increment_Hours:
	mov a, Hours
	add a, #0x01
	da a
	cjne a, #0x24, Print_Hours
	clr a

Print_Hours:
	mov Hours, a
	Set_Cursor(1,5)
	Display_BCD(Hours)
	ljmp Check_Pause
;--------------------------------------------------------------------------------
Increment_Minutes:
	mov a, Minutes
	add a, #0x01
	da a
	cjne a, #0x60, Print_Minutes
	clr a

Print_Minutes:
	mov Minutes, a
	Set_Cursor(1,8)
	Display_BCD(Minutes)
	ljmp Check_Pause
;--------------------------------------------------------------------------------
Increment_Seconds:
	mov a, Seconds
	add a, #0x01
	da a
	cjne a, #0x60, Print_Seconds
	clr a

Print_Seconds:
	mov Seconds, a
	Set_Cursor(1,11)
	Display_BCD(Seconds)
	ljmp Check_Pause
;--------------------------------------------------------------------------------

;---------------;
; ALARM MODE    ;
;---------------;
Check_Alarm:
	jnb ALARM_SWITCH, Change_Alarm ; as long as the switch is on (grounded), check for time changes

	; User wants to return. Clear the screen first
	Set_Cursor(1,1)
	Send_Constant_String(#Empty_Line)
	Set_Cursor(2,1)
	Send_Constant_String(#Empty_Line)

	; Then re-print the clock and exit
	Set_Cursor(1,1)
	Send_Constant_String(#Clock)
	ljmp print

Change_Alarm:
	; BUG: The following instruction should jump to Check_Seconds instead, but this enables a bug where:
	; 1. The seconds counter for alarm 1 or alarm 2 starts at some value and begins incrementing
	jnb ALARM_BUTTON, Check_Minutes_Alarm2 ; if button is held down, user wants to modify weekend alarm instead
;--------------------------------------------------------------------------------
; BUG: The following block of code should not be commented out for the reason above
;Check_Seconds_Alarm1: 
	;jb SEC_BUTTON, Check_Minutes_Alarm1 ; if button not pressed, move on to check minutes
	;Wait_Milli_Seconds(#125)
	;jb SEC_BUTTON, Check_Minutes_Alarm1
	;ljmp Increment_Seconds_Alarm1 ; else, adjust seconds

Check_Minutes_Alarm1:
	jb MIN_BUTTON, Check_Hours_Alarm1
	Wait_Milli_Seconds(#125)
	jb MIN_BUTTON, Check_Hours_Alarm1
	ljmp Increment_Minutes_Alarm1

Check_Hours_Alarm1:
	jb HOUR_BUTTON, Check_Days_Alarm1
	Wait_Milli_Seconds(#125)
	jb HOUR_BUTTON, Check_Days_Alarm1
	ljmp Increment_Hours_Alarm1

Check_Days_Alarm1:
	jb DAY_BUTTON, Jump_To_Check_Alarm_Label1
	Wait_Milli_Seconds(#125)
	jb DAY_BUTTON, Jump_To_Check_Alarm_Label1
	ljmp Increment_Days_Alarm1

; Used since jb/jnb instruction only supports sjmp, not ljmp
Jump_To_Check_Alarm_Label1:
	ljmp Check_Alarm
;--------------------------------------------------------------------------------
Check_Seconds_Alarm2: 
	jb SEC_BUTTON, Check_Minutes_Alarm2 ; if button not pressed, move on to check minutes
	Wait_Milli_Seconds(#125)
	jb SEC_BUTTON, Check_Minutes_Alarm2
	ljmp Increment_Seconds_Alarm2 ; else, adjust seconds

Check_Minutes_Alarm2:
	jb MIN_BUTTON, Check_Hours_Alarm2
	Wait_Milli_Seconds(#125)
	jb MIN_BUTTON, Check_Hours_Alarm2
	ljmp Increment_Minutes_Alarm2

Check_Hours_Alarm2:
	jb HOUR_BUTTON, Check_Days_Alarm2
	Wait_Milli_Seconds(#125)
	jb HOUR_BUTTON, Check_Days_Alarm2
	ljmp Increment_Hours_Alarm2

Check_Days_Alarm2:
	jb DAY_BUTTON, Jump_To_Check_Alarm_Label2
	Wait_Milli_Seconds(#125)
	jb DAY_BUTTON, Jump_To_Check_Alarm_Label2
	ljmp Increment_Days_Alarm2

Jump_To_Check_Alarm_Label2:
	ljmp Check_Alarm
;--------------------------------------------------------------------------------
Increment_Days_Alarm1:
	mov a, Days_Alarm1
	add a, #0x01
	da a
	cjne a, #0x05, Print_Days_Alarm1
	clr a ; Days is cleared only if it overflows to BCD 5 (first weekend day)

Print_Days_Alarm1:
	mov Days_Alarm1, a
	Set_Cursor(1,5)
	Display_day(Days_Alarm1)
	ljmp Check_Alarm
;--------------------------------------------------------------------------------
Increment_Hours_Alarm1:
	mov a, Hours_Alarm1
	add a, #0x01
	da a
	cjne a, #0x24, Print_Hours_Alarm1
	clr a

Print_Hours_Alarm1:
	mov Hours_Alarm1, a
	Set_Cursor(1,9)
	Display_BCD(Hours_Alarm1)
	ljmp Check_Alarm
;--------------------------------------------------------------------------------
Increment_Minutes_Alarm1:
	mov a, Minutes_Alarm1
	add a, #0x01
	da a
	cjne a, #0x60, Print_Minutes_Alarm1
	clr a

Print_Minutes_Alarm1:
	mov Minutes_Alarm1, a
	Set_Cursor(1,12)
	Display_BCD(Minutes_Alarm1)
	ljmp Check_Alarm
;--------------------------------------------------------------------------------
Increment_Seconds_Alarm1:
	mov a, Seconds_Alarm1
	add a, #0x01
	da a
	cjne a, #0x60, Print_Seconds_Alarm1
	clr a

Print_Seconds_Alarm1:
	mov Seconds_Alarm1, a
	Set_Cursor(1,15)
	Display_BCD(Seconds_Alarm1)
	ljmp Check_Alarm
;--------------------------------------------------------------------------------
Increment_Days_Alarm2:
	mov a, Days_Alarm2
	add a, #0x01
	da a
	cjne a, #0x07, Print_Days_Alarm2
	mov a, #0x05 ; Days is reset to 5 (first weekend day) only if it overflows to 7 (first weekday)

Print_Days_Alarm2:
	mov Days_Alarm2, a
	Set_Cursor(2,5)
	Display_day(Days_Alarm2)
	ljmp Check_Alarm
;--------------------------------------------------------------------------------
Increment_Hours_Alarm2:
	mov a, Hours_Alarm2
	add a, #0x01
	da a
	cjne a, #0x24, Print_Hours_Alarm2
	clr a

Print_Hours_Alarm2:
	mov Hours_Alarm2, a
	Set_Cursor(2,9)
	Display_BCD(Hours_Alarm2)
	ljmp Check_Alarm
;--------------------------------------------------------------------------------
Increment_Minutes_Alarm2:
	mov a, Minutes_Alarm2
	add a, #0x01
	da a
	cjne a, #0x60, Print_Minutes_Alarm2
	clr a

Print_Minutes_Alarm2:
	mov Minutes_Alarm2, a
	Set_Cursor(2,12)
	Display_BCD(Minutes_Alarm2)
	ljmp Check_Alarm
;--------------------------------------------------------------------------------
Increment_Seconds_Alarm2:
	mov a, Seconds_Alarm2
	add a, #0x01
	da a
	cjne a, #0x60, Print_Seconds_Alarm2
	clr a

Print_Seconds_Alarm2:
	mov Seconds_Alarm2, a
	Set_Cursor(2,15)
	Display_BCD(Seconds_Alarm2)
	ljmp Check_Alarm
;--------------------------------------------------------------------------------

;---------------;
; ALARM CHECK   ;
;---------------;
Check_Alarm1:
	jb ALARM1_SWITCH, Check_Alarm2 ; Check if alarm 1 is toggled

	; Compare alarm 1 seconds to clock seconds
	mov a, Seconds_Alarm1
	cjne a, Seconds, Check_Alarm2

	mov a, Minutes_Alarm1
	cjne a, Minutes, Check_Alarm2

	mov a, Hours_Alarm1
	cjne a, Hours, Check_Alarm2

	mov a, Days_Alarm1
	cjne a, Days, Check_Alarm2

	sjmp Trigger_Alarm
;--------------------------------------------------------------------------------
Check_Alarm2:
	jb ALARM2_SWITCH, Jump_To_After_Check_Alarm

	mov a, Seconds_Alarm2
	cjne a, Seconds, Jump_To_After_Check_Alarm

	mov a, Minutes_Alarm2
	cjne a, Minutes, Jump_To_After_Check_Alarm

	mov a, Hours_Alarm2
	cjne a, Hours, Jump_To_After_Check_Alarm

	mov a, Days_Alarm2
	cjne a, Days, Jump_To_After_Check_Alarm

	sjmp Trigger_Alarm

; Used since cjne instruction only supports sjmp, not ljmp
Jump_To_After_Check_Alarm:
	ljmp After_Check_Alarm
;--------------------------------------------------------------------------------
; Arrive here if either alarm has been detected
Trigger_Alarm:
	mov a, #0x01
	mov alarm_flag, a

Trigger_Alarm_Loop:
	
	; Make sure to update the clock as the alarm is beeping
	Set_Cursor(1, 11)
	Display_BCD(Seconds)
	Set_Cursor(1, 8)
	Display_BCD(Minutes)
	Set_Cursor(1, 5)
	Display_BCD(Hours)
	Set_Cursor(1, 1)
	Display_day(Days)

	jb ALARM_BUTTON, Trigger_Alarm_loop
	Wait_Milli_Seconds(#125)
	jb ALARM_BUTTON, Trigger_Alarm_loop
	
	mov a, #0x00
	mov alarm_flag, a
	ljmp After_Check_Alarm
;--------------------------------------------------------------------------------

;---------------;
; PROCEDURES    ;
;---------------;
Display_Alarm:
	Set_Cursor(1,1)
	Send_Constant_String(#Weekday_Alarm)
	Set_Cursor(2,1)
	Send_Constant_String(#Weekend_Alarm)

	Set_Cursor(1,5)
	Display_day(Days_Alarm1)
	Set_Cursor(1,9)
	Display_BCD(Hours_Alarm1)
	Set_Cursor(1,12)
	Display_BCD(Minutes_Alarm1)
	Set_Cursor(1,15)
	Display_BCD(Seconds_Alarm1)
	Set_Cursor(2,5)
	Display_day(Days_Alarm2)
	Set_Cursor(2,9)
	Display_BCD(Hours_Alarm2)
	Set_Cursor(2,12)
	Display_BCD(Minutes_Alarm2)
	Set_Cursor(2,15)
	Display_BCD(Seconds_Alarm2)
	ret

END
