#include <iostream>
#include <string>
#include <cstdlib>
#include <sstream>
#include <vector>
#include <math.h>
#include <algorithm>

using namespace std;
bool num_check_is_valid(string num){
	std::istringstream iss(num);
    float f;
    iss >> noskipws >> f; 
    return iss.eof() && !iss.fail(); 
}

int no_of_rows(string mat)
{
	int i;
	int rows=1;
	for(i=0; i<mat.length(); i++)
	{
		if(mat[i]==';')
		{
			rows+=1;
		}
	}
	return rows;
}
vector<vector<float> > Split_Matrices(string matr)
{
	vector<vector<float> > Matrix;
	int rows=no_of_rows(matr);
	Matrix.resize(rows);
	int ref=5;
	for(int i=0; i<rows; i++)
	{
		for(int j=ref; j<matr.length(); j++)
		{
			if(matr[j]==' ')
			{
				string num = matr.substr(ref,j-ref);
				if(num=="-0") num ="0";
				Matrix[i].push_back(atof(num.c_str()));
				ref=1+j;
			}
			else if(matr[j]==';')
			{
				string num = matr.substr(ref,j-ref);
				if(num=="-0") num ="0";
				Matrix[i].push_back(atof(num.c_str()));
				ref=2+j; // 2 as there's space after each ;
				break;
			}
			else if(j==matr.length() -1)
			{
				string num =matr.substr(ref,j-ref);
				if(num=="-0") num ="0";
				Matrix[i].push_back(atof(num.c_str()));
			}
		}
	}
	return Matrix;
}
string GetMatrixDef(string matr){
	for (int i = 0; i < matr.length(); i++)
	{
		if(matr[i]==' '){
			string def = matr.substr(0,i);
			return def;
		}
	}

}
int no_of_columns(vector<vector<float> > ma)
{
	int maxCol=ma[0].size();
	for(int i=1; i<ma.size(); i++)
	{
		if(ma[i].size()>maxCol)
			maxCol=ma[i].size();
	}
	return maxCol;
	
}
bool MatrixIsValid(vector<vector<float> > SMatr)
{
	int rows=SMatr.size();
	int cols=no_of_columns(SMatr);
	int i ,j;
	for(i=0; i<rows; i++)
	{
		for(j=1; j<rows; j++)
		{
			if(SMatr[i].size()!=SMatr[j].size())
				return false;
		}
	}
	return true;
}
int GetDefIndex1(string Operation,vector<string> MatricesDefintions) // For First Operation only
{
	string Definition = Operation.substr(19,Operation.length());
	std::vector<string>::iterator it = std::find(MatricesDefintions.begin(), MatricesDefintions.end(), Definition);
	if (it != MatricesDefintions.end())
	{
		int index = std::distance(MatricesDefintions.begin(), it);
		cout << Definition << " " << "=" << " "; 
		return index;
	}
	return -1;
	
}
int GetDefIndex(string Definition,vector<string> MatricesDefintions) // For First Operation only
{
	std::vector<string>::iterator it = std::find(MatricesDefintions.begin(), MatricesDefintions.end(), Definition);
	if (it != MatricesDefintions.end())
	{
		int index = std::distance(MatricesDefintions.begin(), it);
		//cout << Definition << " " << "=" << " "; 
		return index;
	}
	return -1;
	
}

string GetEquation(string Operation, vector<string > MatricesDefintions)
{

	size_t found = Operation.find(":");
	if (found != string::npos)
	{
		string Equation = Operation.substr(found + 2 , Operation.length());
		return Equation;
	}
	return "ERROR";


}
void PrintMatrix(vector<vector<float > > Matr)
{
	cout<<"[";
	int Rows1=Matr.size();
	int Columns1=no_of_columns(Matr);
	for(int r=0; r<Rows1; r++)
	{
		for(int c=0; c<Columns1; c++)
		{
			cout<<Matr[r][c];
			if(c==Columns1-1&&r!=Rows1-1)
				cout<<";"<<" ";
			else if(c!=Columns1-1)
				cout<<" ";
		}
		if(r==Rows1-1)
			cout<<"]";
	}
}


void getCofactor(vector<vector<float > > A, vector<vector<float> >& temp, int p, int q, int n) 
{ 
    int i = 0, j = 0; 
    // Looping for each element of the matrix 
    for (int row = 0; row < n; row++) 
    { 
        for (int col = 0; col < n; col++) 
        { 
            //  Copying into temporary matrix only those element 
            //  which are not in given row and column 
            if (row != p && col != q) 
            { 
                temp[i][j++] = A[row][col]; 
  
                // Row is filled, so increase row index and 
                // reset col index 
                if (j == n - 1) 
                { 
                    j = 0; 
                    i++; 
                } 
            } 
        } 
    } 
} 
double determinant(vector<vector<float> > A, int n) 
{ 
    double D = 0; // Initialize result 
  
    //  Base case : if matrix contains single element 
    if (n == 1) 
        return A[0][0]; 
  
    vector<vector<float>>  temp; // To store cofactors
	temp.resize(A.size());
	for(int n=0; n<A.size(); n++)
		temp[n].resize(A[n].size());  
    int sign = 1;  // To store sign multiplier 
  
     // Iterate for each element of first row 
    for (int f = 0; f < n; f++) 
    { 
        // Getting Cofactor of A[0][f] 
        getCofactor(A, temp, 0, f, n); 
        D += sign * A[0][f] * determinant(temp, n - 1); 
  
        // terms are to be added with alternate sign 
        sign = -sign; 
    } 
  
    return D; 
} 
void adjoint(vector<vector<float> > A,vector<vector<float> >& adj) 
{ 
    if (A.size() == 1) 
    { 
        adj[0][0] = 1; 
        return; 
    } 
  
    // temp is used to store cofactors of A[][] 
    int sign = 1;
	vector<vector<float> > temp; 
	temp.resize(A.size());
	for(int n=0; n<A.size(); n++)
		temp[n].resize(A[n].size());
  
    for (int i=0; i<A.size(); i++) 
    { 
        for (int j=0; j<A[i].size(); j++) 
        { 
            // Get cofactor of A[i][j] 
           getCofactor(A, temp, i, j, A.size()); 
  
            // sign of adj[j][i] positive if sum of row 
            // and column indexes is even. 
           sign = ((i+j)%2==0)? 1: -1; 
  
            // Interchanging rows and columns to get the 
            // transpose of the cofactor matrix 
            adj[j][i] = (sign)*(determinant(temp, A.size()-1)); 
			//adj[j][i] = (sign)*(det(A.size()-1, temp)); 
        } 
    } 
} 
bool inverse(vector<vector<float> > A, vector<vector<float> > &inverse) 
{ 
    // Find determinant of A[][] 
    double det1 = determinant (A,A.size() ); 
    if (det1 == 0) 
    { 
        cout << "ERROR"; 
        return false; 
    } 
  
    // Find adjoint 
    vector<vector<float>> temp; 
	temp.resize(A.size());
	for(int n=0; n<A.size(); n++)
		temp[n].resize(A[n].size());
    adjoint(A, temp); 
  
    // Find Inverse using formula "inverse(A) = adj(A)/det(A)" 
    for (int i=0; i<A.size(); i++) 
        for (int j=0; j<A.size(); j++) 
            inverse[i][j] = temp[i][j]/float(det1); 
  
    return true; 
} 

int main() {
    string Operation;
	int NumberOfMatrices;
    cout << "Please, Enter Matrices Number: " << endl;
    cin >> NumberOfMatrices;
	cin.ignore(100,'\n');

    vector<vector<float > > NumMatrix;
	vector<vector<vector<float > > > NumMatrices;
	string StringMatrix;
	vector<string> MatricesDefintions; // Symbols
	string Def;
	for (int i = 0; i < NumberOfMatrices; i++)
	{
		cout << "Please, Enter Matrix "<< i <<endl;
		
		getline(cin, StringMatrix);
		NumMatrix = Split_Matrices(StringMatrix);
		Def = GetMatrixDef(StringMatrix);

		NumMatrices.push_back(NumMatrix);
		MatricesDefintions.push_back(Def);

	}

	cout << "enter operation: ";
	getline(cin , Operation);
	while(Operation != "exit" )
	{
		string SubOper = Operation.substr(0,4);
		if(SubOper == "Show"){		
			int index = GetDefIndex1(Operation, MatricesDefintions);
			PrintMatrix(NumMatrices[index]);
		}
		else if(SubOper == "Addi")
		{
			string Equa = GetEquation(Operation, MatricesDefintions);
			int EqualIndex = Equa.find("=");
			string ResultDefMatr = Equa.substr(0,EqualIndex - 1);
			int FirstMatrIndex = Equa.find("+");
			string FirstMatrDef = Equa.substr(EqualIndex + 2, FirstMatrIndex - (EqualIndex + 3));
			int FirstMatrDefIndex = GetDefIndex(FirstMatrDef,MatricesDefintions);
			string SecondMatrDef = Equa.substr(FirstMatrIndex+2  );
			int SecondMatrDefIndex = GetDefIndex(SecondMatrDef,MatricesDefintions);
			int Rows1=NumMatrices[FirstMatrDefIndex].size();
			int Rows2=NumMatrices[SecondMatrDefIndex].size();
			int Columns1 = no_of_columns ( NumMatrices[FirstMatrDefIndex] );
			int Columns2 = no_of_columns ( NumMatrices[SecondMatrDefIndex] );


			if(Rows1==Rows2 && Columns1==Columns2)
			{
				cout<<ResultDefMatr<< " " << "="<< " " << "[";
				for(int r=0; r<Rows1; r++)
				{
					for(int c=0; c<Columns1; c++)
					{
						cout<<NumMatrices[FirstMatrDefIndex][r][c] + NumMatrices[SecondMatrDefIndex][r][c];
						if(c==Columns1-1&&r!=Rows1-1)
							cout<<";"<<" ";
						else if(c!=Columns1-1)
							cout<<" ";
					}
					if(r==Rows1-1)
						cout<<"]";
				}
			
			}
			else
				cout<<"ERROR";
		}
		else if(SubOper == "Subt")
		{
			string Equa = GetEquation(Operation, MatricesDefintions);
			int EqualIndex = Equa.find("=");
			string ResultDefMatr = Equa.substr(0,EqualIndex - 1);
			int FirstMatrIndex = Equa.find("-");
			string FirstMatrDef = Equa.substr(EqualIndex + 2, FirstMatrIndex - (EqualIndex + 3));
			int FirstMatrDefIndex = GetDefIndex(FirstMatrDef,MatricesDefintions);
			string SecondMatrDef = Equa.substr(FirstMatrIndex+2  );
			int SecondMatrDefIndex = GetDefIndex(SecondMatrDef,MatricesDefintions);
			int Rows1=NumMatrices[FirstMatrDefIndex].size();
			int Rows2=NumMatrices[SecondMatrDefIndex].size();
			int Columns1 = no_of_columns ( NumMatrices[FirstMatrDefIndex] );
			int Columns2 = no_of_columns ( NumMatrices[SecondMatrDefIndex] );


			if(Rows1==Rows2 && Columns1==Columns2)
			{
				cout<<ResultDefMatr<< " " << "="<< " " << "[";
				for(int r=0; r<Rows1; r++)
				{
					for(int c=0; c<Columns1; c++)
					{
						cout<<NumMatrices[FirstMatrDefIndex][r][c] - NumMatrices[SecondMatrDefIndex][r][c];
						if(c==Columns1-1&&r!=Rows1-1)
							cout<<";"<<" ";
						else if(c!=Columns1-1)
							cout<<" ";
					}
					if(r==Rows1-1)
						cout<<"]";
				}
			
			}
			else
				cout<<"ERROR";


		}
		else if(SubOper == "Mult")
		{
			string Equa = GetEquation(Operation, MatricesDefintions);
			int EqualIndex = Equa.find("=");
			string ResultDefMatr = Equa.substr(0,EqualIndex - 1);
			int FirstMatrIndex = Equa.find("*");
			string FirstMatrDef = Equa.substr(EqualIndex + 2, FirstMatrIndex - (EqualIndex + 3));
			int FirstMatrDefIndex = GetDefIndex(FirstMatrDef,MatricesDefintions);
			string SecondMatrDef = Equa.substr(FirstMatrIndex+2  );
			int SecondMatrDefIndex = GetDefIndex(SecondMatrDef,MatricesDefintions);
			int Rows1=NumMatrices[FirstMatrDefIndex].size();
			int Rows2=NumMatrices[SecondMatrDefIndex].size();
			int Columns1 = no_of_columns ( NumMatrices[FirstMatrDefIndex] );
			int Columns2 = no_of_columns ( NumMatrices[SecondMatrDefIndex] );
			vector<vector<float> > Matrices3;
			Matrices3.resize(Rows1);
			for(int j1=0; j1<Rows1; j1++)
				Matrices3[j1].resize(Columns2);
			if(Columns1==Rows2)
			{
				for(int r1=0; r1<Rows1; r1++)
				{
					for(int c2=0; c2<Columns2; c2++)
					{
						Matrices3[r1][c2]=0;
						for(int r2=0; r2<Rows2; r2++)
						{
							Matrices3[r1][c2] += NumMatrices[FirstMatrDefIndex][r1][r2] * NumMatrices[SecondMatrDefIndex][r2][c2];
						}
					}
				}
				cout<< ResultDefMatr << " " << "=" << " ";
				PrintMatrix(Matrices3);	
			}
			else
				cout<<"ERROR";
		}
		else if(SubOper == "Tran")
		{
			string Equa = GetEquation(Operation, MatricesDefintions);
			int EqualIndex = Equa.find("=");
			string ResultDefMatr = Equa.substr(0,EqualIndex - 1);
			int FirstMatrIndex = Equa.find("'");
			string FirstMatrDef = Equa.substr(EqualIndex + 2, FirstMatrIndex - (EqualIndex + 2));
			int FirstMatrDefIndex = GetDefIndex(FirstMatrDef,MatricesDefintions);
			
			int Rows1=NumMatrices[FirstMatrDefIndex].size();
			int Columns1 = no_of_columns ( NumMatrices[FirstMatrDefIndex] );
			vector<vector<float> > trans(NumMatrices[FirstMatrDefIndex][0].size(), std::vector<float>(NumMatrices[FirstMatrDefIndex].size()));
			for(int i = 0; i < NumMatrices[FirstMatrDefIndex].size(); i++)
			{
				for(int j = 0; j < NumMatrices[FirstMatrDefIndex][0].size(); j++)
				{
					trans[j][i]=NumMatrices[FirstMatrDefIndex][i][j];
				}
			}
			cout<< ResultDefMatr << " " << "=" << " ";
			PrintMatrix(trans);

		}
		else if(SubOper == "Divi")
		{
			string Equa = GetEquation(Operation, MatricesDefintions);
			int EqualIndex = Equa.find("=");
			string ResultDefMatr = Equa.substr(0,EqualIndex - 1);
			int FirstMatrIndex = Equa.find("/");
			string FirstMatrDef = Equa.substr(EqualIndex + 2, FirstMatrIndex - (EqualIndex + 3));
			int FirstMatrDefIndex = GetDefIndex(FirstMatrDef,MatricesDefintions);
			string SecondMatrDef = Equa.substr(FirstMatrIndex+2  );
			int SecondMatrDefIndex = GetDefIndex(SecondMatrDef,MatricesDefintions);
			int Rows1=NumMatrices[FirstMatrDefIndex].size();
			int Rows2=NumMatrices[SecondMatrDefIndex].size();
			int Columns1 = no_of_columns ( NumMatrices[FirstMatrDefIndex] );
			int Columns2 = no_of_columns ( NumMatrices[SecondMatrDefIndex] );
			vector<vector<float> > Matrices3;
			Matrices3.resize(Rows1);
			for(int j1=0; j1<Rows1; j1++)
				Matrices3[j1].resize(Columns2);
			vector<vector<float> > inv;
			inv.resize(Rows2);
			for(int n=0; n<Rows2; n++)
				inv[n].resize(Columns2);
			if(Rows2==Columns2)
			{
				if( !inverse(NumMatrices[SecondMatrDefIndex],inv))
					cout<<"ERROR";
				else
				{
					if(Columns1==Rows2)
					{
						for(int r1=0; r1<Rows1; r1++)
						{
							for(int c2=0; c2<Columns2; c2++)
							{
								Matrices3[r1][c2]=0;
								for(int r2=0; r2<Rows2; r2++)
								{
									Matrices3[r1][c2] += NumMatrices[FirstMatrDefIndex][r1][r2] * inv[r2][c2];
								}
							}
						}
						cout<< ResultDefMatr << " " << "=" << " ";
						PrintMatrix(Matrices3);
					}
					else
						cout<<"ERROR";
				}
			}
			else
				cout<<"ERROR";
		}

		cout <<endl << "enter operation: ";
		getline(cin , Operation);


		
	}

   
}