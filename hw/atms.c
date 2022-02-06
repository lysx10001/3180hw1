/*
∗ CSCI3180 Principles of Programming Languages
∗
∗ --- Declaration ---
∗
∗ I declare that the assignment here submitted is original except for source
∗ material explicitly acknowledged. I also acknowledge that I am aware of
∗ University policy and regulations on honesty in academic work, and of the
∗ disciplinary guidelines and procedures applicable to breaches of such policy
∗ and regulations, as contained in the website
∗ http://www.cuhk.edu.hk/policy/academichonesty/
∗
∗ Assignment 1
∗ Name : Liu Yunzhi
∗ Student ID : 1155141571
∗ Email Addr : yzliu0@link.cuhk.edu.hk
*/

#include <stdio.h>
#include <stdlib.h>
#include<sys/types.h>
#include<sys/stat.h>
#include<fcntl.h>

int timestamp = 0;
long file_cap = 0;  

FILE *rd_master(); 
char choose_atm();
char check_account(char *account, char *balance, char *negv, FILE *fd);
char choose_operation();
char deposit(char *account, char atm);
char withdrawal(char *account, char *balance, char atm);
char wr_transfer(char *account, long long amount, char operation, char atm);
char transfer(char *account, FILE *fd, char *balance, char atm);
char cont ();

int main (){

    // read master file 

    FILE *fd;
    fd = rd_master();

    // workflow of ATMs

    // step 0 welcome
    printf("##########################################\n");
    printf("##       Gringotts Wizarding Bank       ##\n");
    printf("##               Welcome                ##\n");
    printf("##########################################\n");

    // a loop for continue using
    char cont_use = 'Y';
    while(cont_use=='Y'){

        // step 1 choose a machine
        char chosen_atm=0;
        while (chosen_atm < '0'){
            chosen_atm = choose_atm();
        }

        // step 2 input account and pwd, also check balance
        char account[17];
        char balance[16];
        char *negv=calloc(1,1);
        memset(account,0,sizeof(account));
        memset(balance,0,sizeof(balance));

        // account and pwd
        while (check_account(account, balance, negv, fd)<1);

        // balance
        if (*negv=='-'){
            printf("=> NEGATIVE REMAINS TRANSACTION ABORT\n");
            continue;
        }
        
        // step 3 choose an operation
        char operation=0;
        // check correct operation
        while ((operation=choose_operation())==0);

        // D for deposit
        if (operation=='D')
            while (deposit(account, chosen_atm)==0);
        
        // W for withdrawal
        if (operation=='W')
            while (withdrawal(account, balance, chosen_atm)==0); 

        // T for transfer
        if (operation=='T')
            while (transfer(account, fd, balance, chosen_atm)==0); 
            
        while((cont_use=cont())==0);
    }
    fclose(fd);
    return 0;
}

FILE *rd_master(){

    FILE *fd;
    fd = fopen( "master.txt" , "r" );
    if (fd==NULL){
        printf("Error: can't open master file\n");
        return NULL;
    }

    //check integrity
    if ( fseek(fd,0,SEEK_END) < 0 ){
        printf("Error: can't move offset\n");
        return NULL;
    }
    file_cap = ftell(fd);
    if ( file_cap % 60 != 0 ){
        printf("Error: the master file is broken\n");
        return NULL;
    }
    return fd;
}

char choose_atm(){

    printf("=> PLEASE CHOOSE THE ATM\n");
    printf("=> PRESS 1 FOR ATM 711\n");
    printf("=> PRESS 2 FOR ATM 713\n");

    // check digital and correct choice
    char buf[20];
    fgets(buf,20,stdin);


    if ((buf[1]=='\r' || buf[1]=='\n') && (buf[0]=='1' || buf[0]=='2'))
        return buf[0];
    else{
        printf("=> INVALID INPUT\n");
        return 0;
    }

}

// return 1 if success, otherwise 0
// the pointers are used to store information of current account
char check_account(char *account, char *balance, char *negv, FILE *fd){

    char input_account[17], buf[17], input_pwd[7], pwd[7];
    memset(buf,0,sizeof(buf));
    memset(input_pwd,0,sizeof(input_pwd));
    memset(pwd,0,sizeof(pwd));
    memset(input_account,0,sizeof(input_account));
    printf("=> ACCOUNT\n");
    fgets(input_account, 17, stdin);
    // rubish
    char rubish[100];
    fgets(rubish,100,stdin);
    printf("=> PASSWORD\n");
    fgets(input_pwd, 7, stdin);
    // rubish
    rubish[100];
    fgets(rubish,100,stdin);
    
    // loop for check account by strcmp()
    int offset=-40;
    while(strcmp(input_account,buf)>0){
        offset+=60;
        if (offset>file_cap){
            printf("=> INCORRECT ACCOUNT/PASSWORD\n");
            return 0;
        }
        if (fseek(fd,offset,SEEK_SET)<0){
            printf("Error in fseek\n");
            return 0;
        }
        if (fread(buf,1,16,fd)<0){
            printf("Error in fread\n");
            return 0;
        }
    }

    if (fread(pwd,1,6,fd)<0){
        printf("Error in fread\n");
        return 0;
    }
    // store all information needed
    if (strcmp(input_account,buf)==0 && strcmp(input_pwd,pwd)==0 ){
        strcpy(account,input_account);
        if (fread(negv,1,1,fd)<0){
            printf("Error in fread\n");
            return 0;
        }
        if (fread(balance,1,15,fd)<0){
            printf("Error in fread\n");
            return 0;
        }
        return 1;
    }
    else{
        printf("=> INCORRECT ACCOUNT/PASSWORD\n");
        return 0;
    }

}

char choose_operation(){

    printf("=> PLEASE CHOOSE YOUR SERVICE\n");
    printf("=> PRESS D FOR DEPOSIT\n");
    printf("=> PRESS W FOR WITHDRAWAL\n");
    printf("=> PRESS T FOR TRANSFER\n");

    // check digital and correct choice
    char buf[20];
    fgets(buf,20,stdin);
    if ((buf[1]=='\r' || buf[1]=='\n') && (buf[0]=='D' || buf[0]=='W' ||buf[0]=='T'))
        return buf[0];
    else{
        printf("=> INVALID INPUT\n");
        return 0;
    }

}

// return 1 if success, otherwise 0
char deposit(char *account, char atm){

    printf("=> AMOUNT\n");
    long long amount=0;
    scanf("%lld",&amount);
    amount*=100;
    char rubish[100];
    fgets(rubish,100,stdin);


    if (amount<=0){
        printf("=> INVALID INPUT\n");
        return 0;
    }
    else{
        return wr_transfer(account, amount, 'D', atm);
    }     
}

// return 1 if success, otherwise 0
char withdrawal(char *account, char *balance, char atm){

    printf("=> AMOUNT\n");
    long long amount=0;
    scanf("%lld",&amount);
    amount*=100;
    char rubish[100];
    fgets(rubish,100,stdin);

    long long ibalance = atoll(balance);
    if (amount<=0){
        printf("=> INVALID INPUT\n");
        return 0;
    }
    else if (amount>ibalance){
        printf("=> INSUFFICIENT BALANCE\n");
        return 0;
    }
    else{
        return wr_transfer(account, amount, 'W', atm);
    }
}

// return 1 if success, otherwise 0
char transfer(char *account, FILE *fd, char *balance, char atm){

    printf("=> TARGET ACCOUNT\n");
    char taccount[17];
    memset(taccount,0,sizeof(taccount));
    fgets(taccount,17,stdin);
    // rubish
    char rubish[100];
    fgets(rubish,100,stdin);

    // check self transfer
    if (strcmp(taccount,account)==0){
        printf("=> YOU CANNOT TRANSFER TO YOURSELF\n");
        return 0;
    }

    // check whether target account exist
    char buf[17];
    memset(buf,0,17);
    int offset=-40;
    while(strcmp(taccount,buf)>0){
        offset+=60;
        if (offset>file_cap){
            printf("=> TARGET ACCOUNT DOES NOT EXIST\n");
            return 0;
        }
        if (fseek(fd,offset,SEEK_SET)<0){
            printf("Error in fseek\n");
            return 0;
        }
        if (fread(buf,1,16,fd)<0){
            printf("Error in fread\n");
            return 0;
        }
    }

    // amount
    if (strcmp(taccount,buf)==0){
        
        char flag = 1;
        long long amount=0;
        while (flag==1){
            printf("=> AMOUNT\n");
            scanf("%lld",&amount);
            amount*=100;
            char rubish[100];
            fgets(rubish,100,stdin);

            long long ibalance = atoll(balance);
            if (amount<=0){
                printf("=> INVALID INPUT\n");
                continue;
            }
            else if (amount>ibalance){
                printf("=> INSUFFICIENT BALANCE\n");
                continue;
            }
            else{
                if (wr_transfer(account, amount, 'W', atm)==1)
                    return wr_transfer(taccount, amount, 'D', atm);
            }
        }
    } 
    else{
        printf("=> TARGET ACCOUNT DOES NOT EXIST\n");
        return 0;
    }
        
}

// return 1 if success, otherwise 0
char wr_transfer(char *account, long long amount, char operation, char atm){ 

    // choose atm to write transfer
    FILE *fd= NULL;
        if (atm=='1')
            if ((fd = fopen ("trans711.txt", "a+" ))==NULL){
                printf("Error in open\n");
                return 0;
            }
        if (atm=='2')
            if ((fd = fopen ("trans713.txt", "a+" ))==NULL){
                printf("Error in open\n");
                return 0;
            }        
        

        // write account
        if (fwrite(account,1,16,fd)<0){
            printf("Error in fwrite1\n");
            return 0;
        }

        // write operation
        char buf1[2];
        memset(buf1,0,2);
        buf1[0] = operation;
        if (fwrite(buf1,1,1,fd)<0){
            printf("Error in write2\n");
            return 0;
        }

        // write amount 
        char buf[8];
        memset(buf,0,sizeof(buf));
        lltoa(amount,buf,10);
        int length = strlen(buf);
        for ( int i = 0 ; i < 7-length; i++)
            if (fwrite("0",1,1,fd)<0){
                printf("Error in fwrite3\n");
                return 0;
            }
        if (fwrite(buf,1,length,fd)<0){
            printf("Error in write4\n");
            return 0;
        }

        // write time stamp
        memset(buf,0,sizeof(buf));
        itoa(timestamp,buf,10);
        length = strlen(buf);
        for ( int i = 0 ; i < 5-length; i++)
            if (fwrite("0",1,1,fd)<0){
                printf("Error in write5\n");
                return 0;
            }
        if (fwrite(buf,1,length,fd)<0){
            printf("Error in write6\n");
            return 0;
        }
        timestamp++;

        // write \n
        if (fwrite("\n",1,1,fd)<0){
            printf("Error in write7\n");
            return 0;
        }

        // end
        fclose(fd);
        return 1;
}

char cont (){

    printf("=> CONTINUE?\n");
    printf("=> Y FOR YES\n");
    printf("=> N FOR NO\n");

    // check digital and correct choice
    char buf[20];
    fgets(buf,20,stdin);
    if ((buf[1]=='\r' || buf[1]=='\n') && (buf[0]=='Y' || buf[0]=='N'))
        return buf[0];
    else{
        printf("=> INVALID INPUT\n");
        return 0;
    }
}
