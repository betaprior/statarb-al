#include <Rcpp.h>
#include <Rcpp/RObject.h>
#include <string>
#include <sstream>
#include <map>
#include <iostream>

using std::cout;
using std::endl;
using std::string;
using std::map;
using std::vector;
using std::pair;

template<class T>
string toString(const T& t){
  std::ostringstream stream;
  stream << t;
  return stream.str();
}

template<class T>
T fromString(const string& s){
  std::istringstream stream (s);
  T t;
  stream >> t;
  return t;
}

void get_dims(Rcpp::VectorBase &vec, vector<int> &dims){
    Rcpp::RObject y = vec.attr("dim");
    vector<int> tmp = y.asStdVectorInt();
    for(int i=0; i<tmp.size(); i++){
      dims[i] = tmp[i];
    }
}
  
double get_kth_sigArr_entry(Rcpp::NumericVector &sig2d, int i, int j, int k, int col_offset){
  return sig2d(i, k+j*col_offset);
}

RcppExport SEXP backtest_loop(SEXP r_instr_p, SEXP r_tickers_instrp_idx, 
			      SEXP r_instr_pq, SEXP r_prices_instrpq_idx, SEXP r_dates,
			      SEXP r_pq_factor_list, SEXP r_prices, SEXP r_positions, 
			      SEXP r_sig_mtx, SEXP r_sig_actions, SEXP r_params) {

  SEXP rl=R_NilValue;
  char* exceptionMesg=NULL;
  
  try {
    Rcpp::CharacterVector instr_p(r_instr_p);
    Rcpp::SimpleVector<INTSXP,int> tickers_instrp_idx(r_tickers_instrp_idx);
    Rcpp::CharacterVector instr_pq(r_instr_pq);
    Rcpp::SimpleVector<INTSXP,int> prices_instrpq_idx(r_prices_instrpq_idx);
    Rcpp::CharacterVector dates(r_dates);
    Rcpp::CharacterVector pq_factor_list(r_pq_factor_list);
    Rcpp::NumericVector prices(r_prices);
    Rcpp::NumericVector positions(r_positions);
    Rcpp::NumericVector sig_mtx(r_sig_mtx);
    Rcpp::NumericVector sig_actions(r_sig_actions);
    //    RcppParams params(r_params);
    RcppResultSet rs;
    vector<int> dims_buffer(2,0); //hold dimensions by get_dims
    get_dims(sig_mtx, dims_buffer);
    const int sig_mtx_2dcols = dims_buffer[1];
    static const int SIG_COL_OFFSET = sig_mtx_2dcols / instr_p.size();
    enum sig_arr_idx { SA_S_IDX = 1, SA_K_IDX = 2, SA_BETA_IDX = 8 };

    
    map<string,int> p_name_to_tkr_idx;
    map<string,int> pq_name_to_price_col;

    int foo_int=42;
    
    for (int i = 0; i < instr_p.size(); ++i)
      p_name_to_tkr_idx.insert(std::pair<string,int>(static_cast<string>(instr_p(i)),tickers_instrp_idx(i)));
    for (int i = 0; i < instr_pq.size(); ++i)
      pq_name_to_price_col.insert(std::pair<string,int>(string(instr_pq(i)),prices_instrpq_idx(i)));
    
    cout << "HELLO WORLD" << endl;

    Rprintf("instr_p has length %d\n",instr_p.size());
    Rprintf("instr_pq has length %d\n",instr_pq.size());
    Rprintf("dates has length %d\n",dates.size());
    Rprintf("pq_factor_list has length %d\n",pq_factor_list.size());
    get_dims(pq_factor_list, dims_buffer);
    Rprintf("got dims %d,%d\n",dims_buffer[0],dims_buffer[1]);
    get_dims(prices, dims_buffer);
    Rprintf("prices has dims %d,%d\n",dims_buffer[0],dims_buffer[1]);
    get_dims(positions, dims_buffer);
    Rprintf("positions has dims %d,%d\n",dims_buffer[0],dims_buffer[1]);
    get_dims(sig_mtx, dims_buffer);
    Rprintf("sig_mtx has dims %d,%d\n",dims_buffer[0],dims_buffer[1]);
    get_dims(sig_actions, dims_buffer);
    Rprintf("sig_actions has dims %d,%d\n",dims_buffer[0],dims_buffer[1]);

    int i,j;
    for(i=0;i<1;i++){
      cout << "date: " << string(dates(i)) << endl;
      for(j=0;j<3;j++){
	int tkr_idx = p_name_to_tkr_idx[string(instr_p(j))];
	cout << string(instr_p(j)) << ":" << tkr_idx << " ";
	cout << i << "," << j << ":" << get_kth_sigArr_entry(sig_mtx,i,j,SA_S_IDX,SIG_COL_OFFSET) << endl;
      }
      cout << endl;
    }
    cout << endl;

    map<string,int>::iterator it;
    // showing contents:
    //    std::cout << "pq_name_to_price_col contains:\n";
    // for ( it=pq_name_to_price_col.begin() ; it != pq_name_to_price_col.end(); it++ )
    //   std::cout << (*it).first << " => " << (*it).second << endl;



       // Rprintf("pq_factor_list has length %d,%d\n",pq_factor_list.rows(),pq_factor_list.cols());
    string prefix("test");
    for(i=0;i<2;i++){
      for(j=0;j<2;j++){
	rs.add(prefix + (toString<int>(10*i+j)),(const char*)pq_factor_list(i,j));//Rprintf("%s, ",pq_factor_list(i,j));
      }
    }
    // Rprintf("positions has length %d\n",positions.size());
    
    // rs.add("test",(const char*)pq_factor_list(0));//Rprintf("%s, ",pq_factor_list(i,j));
    // rs.add("test1",(const char*)pq_factor_list(1));//Rprintf("%s, ",pq_factor_list(i,j));
    // rs.add("test2",(const char*)pq_factor_list(0,0));//Rprintf("%s, ",pq_factor_list(i,j));
    // rs.add("test3",(const char*)pq_factor_list(0,1));//Rprintf("%s, ",pq_factor_list(i,j));
    // rs.add("test4",(const char*)pq_factor_list(1,1));//Rprintf("%s, ",pq_factor_list(i,j));


    // RcppMatrix<double> m1(M1);// double mtx based on M1
    // RcppVector<double> v1(V1);// double vector based on V1
    // RcppParams rparam(parms);       // parameter from R based on parms
    // Rcpp::RObject n1sexp(N1);
    // double n1 = n1sexp.asDouble();
    // Rcpp::NumericVector nv1(V1);

    // Rprintf("Printing number:\n%f\n",n1);
    // Rprintf("Printing matrix:\n");
    // for(int i=0; i < m1.rows(); i++){
    //   for(int j=0; j < m1.cols(); j++)
    // 	Rprintf("(%d,%d):%f ",i,j,m1(i,j));
    //   Rprintf("\n");
    // }
    // Rprintf("Printing vector:\n");
    // for(int i=0; i < v1.size(); i++)
    //   Rprintf("%f, ",v1(i));
    // Rprintf("\n");
    // Rprintf("Printing Numeric vector:\n");
    // for(int i=0; i < nv1.size(); i++)
    //   Rprintf("%f, ",nv1(i));
    // Rprintf("\n");

    rl = rs.getReturnList();

  } catch(std::exception& ex) {
    exceptionMesg = copyMessageToR(ex.what());
  } catch(...) {
    exceptionMesg = copyMessageToR("unknown reason");
  }
  
  if (exceptionMesg != NULL)
    Rf_error(exceptionMesg);
    
  return rl;
}



RcppExport SEXP rcpp_test4(SEXP N1, SEXP V1, SEXP M1, SEXP parms) {

  SEXP rl=R_NilValue;
  char* exceptionMesg=NULL;
  
  try {

    RcppMatrix<double> m1(M1);// double mtx based on M1
    RcppVector<double> v1(V1);// double vector based on V1
    RcppParams rparam(parms);       // parameter from R based on parms
    bool verbose = rparam.getBoolValue("verbose");
    RcppResultSet rs;
    Rcpp::RObject n1sexp(N1);
    double n1 = n1sexp.asDouble();
    Rcpp::NumericVector nv1(V1);

    Rprintf("The value of isna is %d\n",R_IsNA(n1));
    Rprintf("The number passed to C++ is %f\n",n1);

    if (verbose) {
      Rprintf("Printing number:\n%f\n",n1);
      Rprintf("Printing matrix:\n");
      for(int i=0; i < m1.rows(); i++){
	for(int j=0; j < m1.cols(); j++)
	  Rprintf("(%d,%d):%f ",i,j,m1(i,j));
	Rprintf("\n");
      }
      Rprintf("Printing vector:\n");
      for(int i=0; i < v1.size(); i++)
	Rprintf("%f, ",v1(i));
      Rprintf("\n");
      Rprintf("Printing Numeric vector:\n");
      for(int i=0; i < nv1.size(); i++)
	Rprintf("%f, ",nv1(i));
      Rprintf("\n");
      }

Rprintf("The matrix object has %d rows and %d columns\n",m1.rows(),m1.cols());

    double element1 = v1(0);
    Rprintf("0 element of the vector is %d\n",element1);
    if(R_IsNA(element1))
      Rprintf("Found NA in vector!\n");
    else
      Rprintf("did not detect NA in vector!\n");

    vector<double> v1_stl(v1.stlVector());
    if(R_IsNA(v1_stl[0]))
      Rprintf("Found NA in STL vector!\n");

    double m00 = m1(0,0);
    Rprintf("00 element of the matrix is %f\n",m00);
    if(R_IsNA(m00))
      Rprintf("Found NA in matrix!\n");
    else
      Rprintf("did not detect NA in matrix!\n");
    
    Rprintf("printing STL matrix:\n");
    vector< vector<double> > m1_stl(m1.stlMatrix());
    for(int i=0; i < m1_stl.size(); i++){
      for(int j=0; j < m1_stl[i].size(); j++)
	Rprintf("(%d,%d):%f ",i,j,m1_stl[i][j]);
      Rprintf("\n");
    }
    if(R_IsNA(m1_stl[0][0]))
      Rprintf("Found NA in STL matrix!\n");
    else
      Rprintf("did not detect NA in STL matrix!\n");

    // rs.add("el1",element1);
    // rs.add("message","dummy return message");
    rl = rs.getReturnList();

  } catch(std::exception& ex) {
    exceptionMesg = copyMessageToR(ex.what());
  } catch(...) {
    exceptionMesg = copyMessageToR("unknown reason");
  }
  
  if (exceptionMesg != NULL)
    Rf_error(exceptionMesg);
    
  return rl;
}

