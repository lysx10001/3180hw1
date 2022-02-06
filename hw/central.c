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
∗ Email Addr : 1155141571@link.cuhk.edu.hk
*/

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>

#ifndef _SORT
#define _SORT
void sort_transaction(char path[], char sort_path[]);

#endif

int merge_transaction();
int update_master();
int change_balance(char *balance, char *amount, char op );

int main (){

    // first sort trans 711 and trans 713
    sort_transaction("trans711.txt","transSorted711.txt");
    sort_transaction("trans713.txt","transSorted713.txt");

    // second merge transactions
    // complete the function below
    if (merge_transaction()<0){
        printf("Error in merge transactions\n");
        return -1;
    }
        

    // third update master file
    if (update_master()<0){
        printf("Error in merge transactions\n");
        return -1;
    }

    // check negv account
    


}

int merge_transaction(){

    FILE *fd711;
    FILE *fd713;
    FILE *fd;   

    // open and get file capacity
    fd711 = fopen( "transSorted711.txt" , "r" );
    fd713 = fopen( "transSorted713.txt" , "r" );
    fd   = fopen( "transSorted.txt" , "a" );

    fseek(fd711,0,SEEK_END);
    fseek(fd713,0,SEEK_END);

    int file_cap711 = ftell(fd711);
    int file_cap713 = ftell(fd713);
    fseek(fd711,0,SEEK_SET);
    fseek(fd713,0,SEEK_SET);

    // merge 
    char buf711acc[17];
    char buf711tst[6];
    memset(buf711acc,0,17);
    memset(buf711tst,0,6);

    char buf713acc[17];
    char buf713tst[6];
    memset(buf713acc,0,17);
    memset(buf713tst,0,6);

    char buf[31];
    memset(buf,0,31);

    while ( ftell(fd711) < file_cap711 || ftell(fd713) < file_cap713 ){

        // 711 run out
        if (ftell(fd711) >= file_cap711){
            fgets(buf,31,fd713);
            buf[39] = '\n';
            fwrite(buf,1,30,fd);
            memset(buf,0,31);
        }

        // 713 run out
        else if (ftell(fd713) >= file_cap713){
            fgets(buf,31,fd711);
            buf[29] = '\n';
            fwrite(buf,1,30,fd);
            memset(buf,0,31);
        }

        else{

            fread(buf711acc,1,16,fd711);
            fread(buf713acc,1,16,fd713);

            // compare account
            if (strcmp(buf711acc,buf713acc) == 0 ){

                fseek(fd711,8,SEEK_CUR);
                fseek(fd713,8,SEEK_CUR);
                // compare timestamp
                fread(buf711tst,1,5,fd711);
                fread(buf713tst,1,5,fd713);
                int test = fseek(fd711,-29,SEEK_CUR);
                test = fseek(fd713,-29,SEEK_CUR);

                if (strcmp(buf711tst,buf713tst) > 0 ){
                    fgets(buf,31,fd713);
                    buf[29] = '\n';
                    fwrite(buf,1,30,fd);                                                                  
                }
                else{
                    fgets(buf,31,fd711);
                    buf[29] = '\n';
                    fwrite(buf,1,30,fd);
                }
                memset(buf,0,31);   
            }
            else {
                fseek(fd711,-16,SEEK_CUR);
                fseek(fd713,-16,SEEK_CUR);
                if (strcmp(buf711acc,buf713acc) > 0){
                    fgets(buf,31,fd713);
                    buf[29] = '\n';
                    fwrite(buf,1,30,fd);
                }
                else {
                    fgets(buf,31,fd711);
                    buf[29] = '\n';
                    fwrite(buf,1,30,fd);
                }
                memset(buf,0,31);   
            }
        }
    }
    fclose(fd713);
    fclose(fd711);
    fclose(fd);
    return 0;
}

int update_master(){
    FILE *fd_m;
    FILE *fd_um;
    FILE *fd_ts;
    FILE *fd_nr;
    fd_nr=fopen("negReport.txt","a");
    long  mlength, tlength;
    if ((fd_m = fopen( "master.txt" , "r" ))==NULL){
        printf("Error in open master.txt\n");
        return -1;
    }
    if ((fd_um = fopen( "updatedMaster.txt" , "a" ))==NULL){
        printf("Error in open “updatedMaster.txt\n");
        return -1;
    }
    if ((fd_ts = fopen( "transSorted.txt" , "r" ))==NULL){
        printf("Error in open transSorted.txt\n");
        return -1;
    }

    if (fseek(fd_m,0,SEEK_END)<0){
        printf("Error in move offset1\n");
        return -1;
    }
    mlength=ftell(fd_m);
    rewind(fd_m);

        if (fseek(fd_ts,0,SEEK_END)<0){
        printf("Error in move offset3\n");
        return -1;
    }
    tlength=ftell(fd_ts)/31*30;
    rewind(fd_ts);
    char buf1[300];
    fread(buf1,1,300,fd_ts);
    rewind(fd_ts);
    int ml ;
    int tl ;
    //
    while ( (ml = ftell(fd_m) )<mlength){

        char mbuf[60];
        memset(mbuf,0,60);
        if (fgets(mbuf,60,fd_m)<0){
            printf("Error in read master.txt\n");
            return -1;
        }
        if (mbuf[0]<'0')
            break;
        char account[17],balance[16];
        memset(account,0,17);
        memset(balance,0,16);
        strncpy(account,mbuf+20,16);
        strncpy(balance,mbuf+43,15);

        while ((tl = ftell(fd_ts))< tlength){
            char tbuf[30],tacc[17],amount[8];
            memset(tacc,0,17);
            memset(amount,0,8);
            fread(tbuf,1,30,fd_ts);
            if (tbuf[0]<'0')
                break;
            strncpy(tacc,tbuf,16);
            if (strcmp(account,tacc)<0){
                if (fseek(fd_ts,-31,SEEK_CUR)<0){
                    printf("Error in move offset5\n");
                    return -1;
                }
                break;
            }
            else{
                strncpy(amount,tbuf+17,7);
                if ((mbuf[42] == '+' && tbuf[16] == 'D') || (mbuf[42] == '-' && tbuf[16] == 'W')){
                    change_balance(balance, amount, '+');
                    strncpy(mbuf+43,balance,15);
                }
                if ((mbuf[42] == '+' && tbuf[16] == 'W') || (mbuf[42] == '-' && tbuf[16] == 'D')){
                    int prefix = change_balance(balance, amount, '-');
                    if (prefix==1)
                        mbuf[42] = '+';
                    else if (prefix==2){
                        if (mbuf[42] == '+')
                            mbuf[42] = '-';
                        else
                            mbuf[42] = '+';
                    }
                    strncpy(mbuf+43,balance,15);
                }
            }
        }
        if (fputs(mbuf,fd_um)<0){
            printf("Error in write updatedMaster.txt\n");
            return -1;
        }
        if (mbuf[42]=='-'){
            char name[20],account[16],balance[16];

            strncpy(name,mbuf,20);
            strncpy(account,mbuf+20,16);
            strncpy(balance,mbuf+42,16);
            fprintf(fd_nr,"Name: %.20sAccount Number: %.16sBalance: %.16s\n",name,account,balance);
        }

    }
    fclose(fd_m);
    fclose(fd_ts);
    fclose(fd_um);
    fclose(fd_nr);

    return 0;
}


int change_balance(char *balance, char *amount, char op ){

    long long lbalance,lamount,sum;
    lbalance=atoll(balance);
    lamount=atoll(amount);
    if (op == '+')
        sum=lbalance+lamount;
    if (op == '-' )
        sum=lbalance-lamount;

    char nev;
    if ( sum > 0)
        nev=0;
    else if (sum<0){
        sum=-sum;
        nev=2;
    }
    else   
        nev=1;

    lltoa (sum,balance,10);
    int length = strlen(balance);
    for ( int i = 0 ; i < length ; i++ )
        balance[i+15-length] = balance[i] ;
    for ( int i = 0 ; i < 15 - length ; i++ )
        balance[i]='0';
    
    if (nev == 0)
        return 0;
    if (nev == 1 )
        return 1;
    if (nev == 2 )
        return 2;
}










































void read_str(char input_line[], char output_line[], int start_index, int length) {
    strncpy(output_line, input_line + start_index, length);
    output_line[length] = '\0';
}

////////////////////////////////////////////////
// global variable
int MAX_TRAN = 10;

struct transaction {
    char transac_account[20];
    char others[15];
    char timestamp[10];
};

struct transaction * process_one_transaction(char line[]) {
    struct transaction * result_transaction = (struct transaction *) malloc(sizeof(struct transaction));
    
    char temp_str[30];

    // transaction account
    read_str(line, temp_str, 0, 16);
    strcpy(result_transaction->transac_account , temp_str);

    // operation
    read_str(line, temp_str, 16, 8);
    strcpy(result_transaction->others , temp_str);

    // timestamp 
    read_str(line, temp_str, 24, 5);
    strcpy(result_transaction->timestamp , temp_str);

    return result_transaction;
}

struct transaction ** get_transactions(char stat_path[]){
    struct transaction ** all_transactions = (struct transaction **) malloc(sizeof(struct transaction *) * MAX_TRAN);
    FILE * fp = fopen(stat_path, "r");
    for (int i = 0; i < MAX_TRAN; i++) all_transactions[i] = NULL;
    char line[60];
    int cnt = 0;
    while (fgets(line, 60, fp) != NULL) {
        all_transactions[cnt] = process_one_transaction(line);
        cnt += 1;

        if (cnt == MAX_TRAN) {
            // allocate more space
            MAX_TRAN *= 2;
            struct transaction ** all_transactions_temp;
            all_transactions_temp = realloc(all_transactions, sizeof(struct transaction *) * MAX_TRAN);
            all_transactions = all_transactions_temp;
        }
    }
    return all_transactions;

}

void swap(struct transaction *a , struct transaction *b)
{
    struct transaction temp ;
    temp = *a ;
    *a = *b ;
    *b = temp ;
    return ;
}

struct transaction ** sort_transactions(struct transaction ** transaction_i){

    int transaction_index = 0, temp_index = 0;
    while (transaction_i[transaction_index] != NULL) {
        temp_index = transaction_index;
        while (transaction_i[temp_index] != NULL){
            if (strcmp(transaction_i[temp_index]->transac_account,transaction_i[transaction_index]->transac_account)<0){
                swap(transaction_i[temp_index], transaction_i[transaction_index]);
            }
            else{
                if (strcmp(transaction_i[temp_index]->transac_account,transaction_i[transaction_index]->transac_account)==0){
                    if (strcmp(transaction_i[temp_index]->timestamp,transaction_i[transaction_index]->timestamp)<0){
                        swap(transaction_i[temp_index], transaction_i[transaction_index]);
                    }
                }
            }
            temp_index += 1;

        }
        transaction_index += 1;
    }
    return transaction_i;
}

void save_transactions(struct transaction ** transactions, char save_path[]){
    FILE * fp = fopen(save_path, "w");
    int transaction_index = 0;
    while(transactions[transaction_index] != NULL){
        fprintf(fp, "%.16s%.8s%.5s", transactions[transaction_index]->transac_account, transactions[transaction_index]->others, transactions[transaction_index]->timestamp);
        fprintf(fp, "%s", "\n");
        transaction_index += 1;
    }
    fclose(fp);
}

void sort_transaction(char path[], char sort_path[]){
    struct transaction ** transactions = get_transactions(path);
    struct transaction ** transactions_sort = sort_transactions(transactions);
    save_transactions(transactions_sort, sort_path);
}
///////////////////////////////////////////////