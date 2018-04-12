       program-id. Program1.
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "master.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANS-FILE ASSIGN TO "trans.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO "OUTPUT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       data division.
       FILE SECTION.
       FD MASTER-FILE.
           01 MASTER-REC.
               05 M-PROD-ID            PIC XX.
               05 M-DESC               PIC X(15).
               05 M-QT                 PIC 99.
               
       FD TRANS-FILE.
           01 TRANS-REC.
               05 T-DATE               PIC 9(8).
               05 T-PROD-ID            PIC xx.
               05 T-QT                 PIC 99.
               05 T-ACTION             PIC X.
       
       FD REPORT-FILE.
           01 REPORT-REC.
               05 R-PROD-ID            PIC XX.
               05 R-DESC               PIC X(15).
               05 R-QT                 PIC 99.
           
       working-storage section.
       01 EOF                          pic X VALUE "N".
       
       01 WS-CONTROL-KEY               pic 99.
       
       01 WS-ALLOCATED-SWITCH          pic X.
       
       procedure division.

       100-MAIN.
           open input MASTER-FILE, TRANS-FILE
               output REPORT-FILE.
               
           perform 125-READ-MASTER
           perform 150-READ-TRANS
           perform 175-CONTROL-KEY
           perform 200-COMPARE UNTIL T-PROD-ID = "99" AND M-PROD-ID = "99"
           
           close MASTER-FILE
               TRANS-FILE
               REPORT-FILE.
               
               stop run.
               
               
       125-READ-MASTER.
           read MASTER-FILE
               at end move "99" to M-PROD-ID.
       display MASTER-REC.
       
       150-READ-TRANS.
           read TRANS-FILE
               at end move "99" to T-PROD-ID.
               
       175-CONTROL-KEY.
           if T-PROD-ID < M-PROD-ID
               move T-PROD-ID TO WS-CONTROL-KEY
           else    
               move M-PROD-ID to WS-CONTROL-KEY
           end-if.
           
      
       
       200-COMPARE.
           if M-PROD-ID = WS-CONTROL-KEY
               move "Y" to WS-ALLOCATED-SWITCH
               perform 300-CREATE-NEW-MASTER
           ELSE
               move "N" to WS-ALLOCATED-SWITCH
           END-IF
           
           perform 400-PROCESS-TRANSACTION UNTIL 
               WS-CONTROL-KEY NOT EQUAL TO T-PROD-ID.
           if WS-ALLOCATED-SWITCH = "Y"
           write REPORT-REC.
           perform 175-control-key.
       
       
       300-CREATE-NEW-MASTER.
       move MASTER-REC to REPORT-REC

           perform 125-READ-MASTER.
       
       400-PROCESS-TRANSACTION.
       
           if T-ACTION = "A" perform 500-ADD-RECORD
           ELSE
           if T-ACTION = "U" perform 700-UPDATE-RECORD
           ELSE
           if T-ACTION = "D" perform 600-DELETE-RECORD
           
           END-IF.
           
           perform 150-READ-TRANS.
           
         
       500-ADD-RECORD.
           if WS-ALLOCATED-SWITCH = "Y"

       else
           move "Y" to WS-ALLOCATED-SWITCH
               move spaces to R-DESC
               move T-PROD-ID to R-PROD-ID
               move T-QT to R-QT.
       
       600-DELETE-RECORD.
           if WS-ALLOCATED-SWITCH ="Y"
           move "N" to WS-ALLOCATED-SWITCH.
           
       
       700-UPDATE-RECORD.
           if WS-ALLOCATED-SWITCH = "Y"
               COMPUTE R-QT = M-QT - T-QT.
  
               
               
       
       
               
        
           

