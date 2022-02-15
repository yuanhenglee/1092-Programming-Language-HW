#include<bits/stdc++.h>

using namespace std;

int findMinDistance( string str1, string str2){
    int n = str1.length()+1;
    int m = str2.length()+1;

    //initialize dp array
    int** dp = (int**)malloc( n * sizeof(int*) );
    for( int i= 0 ; i < n ; i++ ){
        dp[i] = (int*)malloc( m * sizeof(int) );
        for( int j= 0 ; j < m ; j++){
            if( i == 0 ) dp[i][j] = j;
            else if( j == 0 ) dp[i][j] = i;
            else dp[i][j] = -1;
        }
    }

    //find min distance
    for( int i=1 ; i<n ; i++ ){
        for( int j=1 ; j<m ; j++ ){
            if( str1[i-1] == str2[j-1] ){
                dp[i][j] = dp[i-1][j-1];
            }else{
                dp[i][j] = min( min(dp[i-1][j-1],dp[i][j-1]) , dp[i-1][j] ) + 1;
            }
        }
    }

    return dp[n-1][m-1];

}

int main(int argc, char** argv){

    if( argc != 3 ) cout<<"USAGE: minDistance.exe \"string1\" \"string2\" "<<endl;
    else{
        cout<<"The min distance between "<<argv[1]<<" and "<<argv[2]<<" is\n";
        cout<<findMinDistance(argv[1],argv[2])<<endl;
    }


    return 0;
}