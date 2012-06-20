#include<stdio.h>
#include<stdlib.h>
#include <string.h>
#include <jconv.h>

//渡されてきたデータを読み取り変換を実行するメソッド
//入力：順番に、「入力文字MAX、入力文字」変換後文字、変換文字MAX、変換後のバイト数、リターンコード」（文字列型）
//出力：順番に、「変換後の文字、変換後のバイト数、リターンコード」（文字列型）
extern int orcsjistoeuc(char *args){
        int inlen, outlen;
        int *maxlen;
        char *indata;char *outdata;int *ret;
int count = 0;

inlen = *((int *)(args + count));
count += sizeof(int *);

indata = (args + count);
count += 10000;

outdata = (args + count);
count += 20000;

outlen = *((int *)(args + count));
count += sizeof(int *);

maxlen = ((int *)(args + count));
count += sizeof(int *);

ret = ((int *)(args + count));

        sjis_to_euc(inlen, indata, outdata, outlen, maxlen, ret);


return 0;
}

//SJISコードで書かれた文字列をEUCコードに変換するメソッド
//入力：左から順番に「inlen(入力文字MAX),indata(入力文字),outdata(変換後の文字),outlen(変換文字MAX),maxlen(変換後のバイト数),ret(リターンコード)」
int sjis_to_euc(int inlen, char *indata, char *outdata,
         int outlen, int *maxlen,int *ret){
 char *euc_data;
 int count = 0;
 int count2 = 0;
 int i = 0;
 int ret_flag = 0;
 int str_len;
 unsigned char str1,str2;
 unsigned char *str;
 unsigned char tr_str[4];
 unsigned char str_data[20000];
 char strdata[3];
 unsigned char *pnt_str1,*pnt_str2;  
 
   StringCobol2C(indata, inlen);
         *maxlen = strlen(indata);
 while(i <= *maxlen){
 str1 = *(indata + count);
 str2 = *(indata + count + 1);
 if(((0x00 < str1) && (str1 <= 0x7F)) || ((0xA0 <= str1) && (str1 <= 0xDF))){
count += 1;
 }else if(((str1 >= 0x80) && (str1 <= 0x9F)) || ((str1 >= 0xE0) && (str1 <= 0xFC))){
count += 2;
ret_flag = 1;
break;
}else{
str1 = 0x20;
count += 1;
}
i++;
 }
         euc_data = convert_kanji_strict(indata, "EUCJP", "SJIS");
 if((!strcmp(euc_data,indata)) && (ret_flag == 1)){
 *ret = 1;
 count = 0;
 count2 = 0;
 while(count <= *maxlen){
 ret_flag = 0;
  count2 = count;
  str1 = *(indata + count);
 str2 = *(indata + count + 1);
 if(((0x00 < str1) && (str1 <= 0x7F)) || ((0xA0 <= str1) && (str1 <= 0xDF))){
 count += 1;
 }else if(((0x80 <= str1) && (str1 <= 0x9F)) || ((0xE0 <= str1) && (str1 <= 0xFC))){
if((str1 == 0x87) && ((0x40 <= str2) && (str2 <= 0x9C))){
str1 = 0x20;
str2 = 0x20;
}
strdata[0] = str1;
strdata[1] = str2;
strdata[2] = '\0';
ret_flag = 1;
 count += 2;
 }else{
 str1 = 0x20;
 count += 1;
 }
 if(ret_flag == 0){
 str = convert_kanji_strict(&str1, "EUCJP", "SJIS");
 }else{
 str = convert_kanji_strict(&strdata[0], "EUCJP", "SJIS");
 }
 str_len = strlen(str);
 memcpy((char *)(str_data + count2), str, str_len);
 free(str);
 }
 strcpy(outdata, str_data);
 }else{
 *ret = 0;
 strcpy(outdata, euc_data);
 }
 free(euc_data);
// StringC2Cobol(outdata, outlen);
*maxlen = StringC2Cobol(outdata, outlen);
}


//COBOL形式の文字列データをC形式に変換するメソッド
//入力：左から順番に「str(変換する文字列),size(文字列MAX)」
int StringCobol2C(char *str, size_t size){
         char *p;
         for( p = str + size - 1 ; p >= str ; p -- ) {
                   if (  *p  ==  ' '  ) {
                            *p = 0;
                   } else if (  *p  ==  0  ) {
                            /*      */
                   } else {
                            break;
                   }
         }
 return 0;
}


//C形式の文字列データをC形式に変換するメソッド
////入力：左から順番に「str(変換する文字列),size(文字列MAX)」
int StringC2Cobol(char *str, size_t size){
	int i;
	int cnt;
	int fEnd;
	fEnd = 0;
	cnt = 0;
	for( i = 0 ; i < size ; i ++, str ++ ) {
		if( fEnd ) {
			*str = ' ';
		} else if(*str == 0) {
			fEnd = 1;
		} else {
			cnt++ ;
			}
	}
	return cnt;
}

