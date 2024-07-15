.model small  ; Declare the memory model as small

.data 
    ARR DW 10 DUP(?)          ;arr to put palindrome and prime number in it , the size of it is 10
    DATA1 DW 2D               ;start from 2d
    DATA2 DW 10               ;DATA2 will use for divide the number to 10 to reverse it 
    DATA3 DW 1D               ;DATA3 use for reversing the number
    COPYD1 DW 0D              ;COPYD1 save the number of DATA1 because we increase the DATA1 a little soon so we need the value of it
    QOUT DW ?                 ;QOUT is Keeps the divisor after each division
    REMAIN DW ?               ;REMAIN is used for keep the remain in div for reversing the number
    MULT DW ?                 ;MULT is reverse number
    FLAG DW 0D                ;FLAG is for check the iteration in reversing is 1 or more
    CONT DW 0                 ;CONT is the counter of palindrome and prime numbers
    I DW 2                    ;I is the counter of prime checking                                                  
    REMAINFORPRIME DW 0       ;REMAINFORPRIME keep the remein in the prime loop                                    
    PALINPRIMEFLAG DW 0       ;PALINPRIMEFLAG is for define the number is palindrome and prime or not              
                                                                                                                   
                                                                                                                   
.code                                                                                                              
main proc                                                                                                          
    MOV AX, @DATA             ;start the segment                                                                   
    MOV DS,AX                 ;put ax in dx                                                                        
    MOV SI,OFFSET ARR         ;save the address of first index of our array                                        
  
  
    LP:                       ;a loop for check numbers for palindrome and prime
    MOV PALINPRIMEFLAG,0      ;put zero in PALINPRIMEFLAGat at the start of process of operations for each number
    MOV DATA3,1D              ;put 1 in DATA3 at the first because it became 10 from the last number operation
    MOV MULT,0                ;put 0 in MULT,because it became the reverse of the last number and should become reset
    MOV AX,DATA1              ;put DATA1 in AX
    CALL ISPALINDROME         ;call ISOALINDROME proc for check if a number is oalindrome or not
    CMP PALINPRIMEFLAG,1      ;chack PALINPRIMEFLAG is 1 or not, if it is 1, it means the number is palindrome and prime
    JNE NEXT_NUM              ;if it isnt palindrome and prime jump to NEXT_NUM for check the conditions
    MOV AX,COPYD1             ;if it is,put the number in AX
    MOV [SI],AX               ;put the number in the arr
    INC SI                    ;1 byte add to SI
    INC SI                    ;1 byte add ti SI
     
 

    NEXT_NUM:                 ;NEXT_NUM label to check if we found 10 number, if not jump to LP again for next number
    CMP CONT,10               ;check if CONT(counter of palindrome and prime numbers) is 10 or nit
    JNE LP                    ;if not,check next number
    MOV AH, 4Ch               ;finish the number
    INT 21h 
    
    RET   

main endp  

ISPALINDROME PROC             ;ISPALINDROME proc for operations of check a number is palindrome
REVERSE_LOOP:                 ;REVERSE_LOOP label is for a loop to reverse our number at first
    SUB DX,DX                 ;sub dx from dx to make dx=0 to have the remain of the div correctly
    DIV DATA2                 ;ax=ax/data2  dx=ax%data2
    MOV QOUT,AX               ;put AX in QOUT
    MOV REMAIN,DX             ;put DX in REMAIN
    MOV AX,MULT               ;put mult(reverse number until now) in AX
                              
                               
                             
    MOV CX,DATA3              ;put DATA3 in CX(if it is the first iteration , data3 should be 1 to only have the remain but in next iterations make it 10)             
    MUL CX                    ;AX(reverse number)=AX*CX
    MOV MULT,AX               ;put AX(reverse number until now) in MULT=>shift the reverse number
    ADD AX,REMAIN             ;AX=Ax+REMAIN
    MOV MULT,AX               ;update the reverse number
    
    CMP FLAG,1                ;check FLAG with 1 , actually we put this to check it is the first iteration or not for change the data3
    JE DONT_MULT              ;if it was 1 , we dont need to *10 and it is 10 now and correct,so jump to DONT_MULT
    MOV AX,DATA3              ;if it was 0, we are in first iteration ans should *10.
    MOV CX,10                                 
    MUL CX                    ;AX=Ax*CX
    MOV DATA3,AX              ;DATA3=Ax
    MOV FLAG,1                ;make the FLAG=1
    
    
    
 DONT_MULT:                   ;DONT_MULT label for the continuation of the reverse operation
    MOV AX,QOUT               ;AX=QOUT ,it is for if the operation continues, the divided number should be placed in the continuation of the operation
    CMP QOUT,0                ;check if our divided number become 0 , reversing operation should be done
    JNE REVERSE_LOOP          ;if still our number has digit,jump to the REVERSE_LOOP
    
   
    MOV FLAG,0                ;in this part , reversing operation is done and FLAG should be 0 for next numbers
    MOV AX,DATA1              ;put DATA1 in AX to save the current number in COPYD1
    MOV COPYD1,AX             ;put AX in COPYD1
    INC DATA1                 ;DATA1=DATA1+1
    CMP AX,MULT               ;check if the reverse number and the number are equal or not
    JE  PALIN                 ;if they are equal, jump to PALIN label to call the ISPRIME
     
    JMP ENDPROC               ;jump to ENDPROC because the number is not palindrome
     

 PALIN:                       ;call ISPRIME proc to check the number is prime too?
      CALL ISPRIME 
      
ENDPROC:                      ;end of process for the current number
  
RET                           ;return
      
ISPALINDROME ENDP
            
            
            
            
            
ISPRIME PROC                  ;ISPRIMR proc for prime operation
    MOV I,2D                  ;put 2 in I(the counter of the loop)
    CMP COPYD1,2              ;check if the number is 2 
    JE YES                    ;if it is, jump to YES(it is prime)
    MOV AX,COPYD1
    DIV I                     ;check if the number is even
    CMP DX,0
    JE NO 
    INC I
    FOR:                      ;FOR label, a for loop for check the remain from 2 to COPYD1-1
    MOV AX,I                  ;put the I(counter) in AX
    CMP AX,COPYD1             ;It checks whether we have checked all the numbers smaller than it 
    JE  YES                   ;it we check, then our number is prime , jump to YES label
    MOV AX,COPYD1             ;we do not check all numbers, so put COPYD1 in AX for div
    SUB DX,DX                 ;sun DX from DX o make it 0
    DIV I                     ;AX=AX/I DX=AX%I
    MOV REMAINFORPRIME,DX     ;put dx in REMAINFORPRIME
    CMP REMAINFORPRIME,0      ;check the remain is 0 or not
    JE NO                     ;if it is 0, it means the number is not prime so jump to NO
    INC I 
    INC I                    ;increase the counter
    JMP FOR                   ;jump to FOR
    


    YES:                      ;our number is prime,put 1 in PALINPRIMEFLAG to declear it is prime
    MOV PALINPRIMEFLAG ,1     ;INC  CONT(counter of palindrome and prime)
    INC CONT  
  
  
    NO:                       ;our number is not prime
    RET                       ;return
    
ISPRIME ENDP 
