#include <Rcpp.h>
#include <Rcpp/RObject.h>
#include <string>
#include <sstream>
#include <map>
#include <iostream>
#include <cmath>

using std::cout;
using std::endl;
using std::string;
using std::map;
using std::vector;
using std::pair;

inline double max(double x, double y){ return (x<=y) ? y : x;  };

// rounding:
// cf http://www.cplusplus.com/forum/articles/3638/
long cint(double x){
  double foo;
  if (modf(x,&foo)>=.5)
    return x>=0?ceil(x):floor(x);
  else
    return x<0?ceil(x):floor(x);
}

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
    vector<int> tmp(Rcpp::as< vector<int> >(y));
    for(int i=0; i<tmp.size(); i++){
      dims[i] = tmp[i];
    }
}

vector<int> get_dimensions(Rcpp::VectorBase &vec){
    Rcpp::RObject y = vec.attr("dim");
    return(Rcpp::as< vector<int> >(y));
}


template <typename T, int RTYPE>
void col_sums(Rcpp::SimpleVector<RTYPE> &vec, vector<T> &result){
  vector<int> dims_buffer(2,0); //hold dimensions by get_dims
  get_dims(vec, dims_buffer);
  for(int col=0; col < dims_buffer[1]; col++){
    result[col] = 0;
    for(int row=0; row < dims_buffer[0]; row++)
      result[col] += vec(row,col);
  }
}

string int_to_binary( unsigned long n ) {
  // http://www.cplusplus.com/forum/general/10898/
  char     result[ (sizeof( unsigned long ) * 8) + 1 ];
  unsigned index  = sizeof( unsigned long ) * 8;
  result[ index ] = '\0';

  do result[ --index ] = '0' + (n & 1);
  while (n >>= 1);

  return string( result + index );
}

template <typename Array>
double vectorSum(Array a, long count) { // Array can be a pointer or an iterator
  double sum = 0.0;
  for (long i = 0; i<count; ++i) 
    sum += a[i];
  return sum;                  
} 

  
double get_kth_sigArr_entry(Rcpp::NumericVector &sig2d, int i, int j, int k, int col_offset){
  return sig2d(i, k+j*col_offset);
}

void get_kth_sigArr_slice(Rcpp::NumericVector &sig2d, int i, int j, int k,
			  int col_offset, int len, vector<double> &buf){
  for(int kk = k; kk < k + len; kk++)
    buf[kk-k] = sig2d(i, kk+j*col_offset);
}

// void trade_instruments(int j, Rcpp::NumericVector &sig_mtx,
// 		       Rcpp::NumericVector &sig_actions, double &cash, 
// 		       Rcpp::SimpleVector<INTSXP,int> &positions_p,
// 		       Rcpp::SimpleVector<INTSXP,int> &positions){

// }


/* 
 * Function backtest_loop
 * Inputs:
 * - r_instr_p, r_tickers_instrp_idx
 *     list of "P" instruments and a correspondence map (instr_p -> tickers)
 *     (correspondence map M: A -> B contains M[i] s.t. A[i] ~ B[ M[i] ])
 *     NB: this correspondence map is used for PQ factor list in the non-PCA case!
 * - r_pca
 *     boolean flag (PCA or not?)
 * - r_instr_pq, r_prices_instrpq_idx
 *     list of "PQ" (P union Q) instruments and a correspondence map
 *     (instr_pq -> prices).  
 *     NB: same correspondence map is used for positions!
 * - r_q_alloc_mtx, r_prices_qalloc_idx
 *     In the PCA case (pca flag set), these contain allocations matrices and
 *     the (allocations -> prices) correspondence map
 *     In the non-PCA case, r_q_alloc_mtx contains the ticker/ETF corr. list,
 *     aligned with instr_p array and accessed by its corr. map
 * - r_prices, r_positions, r_positions_p
 * - r_sig_mtx, r_sig_actions, r_params
 */
RcppExport SEXP backtest_loop_pca(SEXP r_instr_p, SEXP r_tickers_instrp_idx, SEXP r_pca,
				  SEXP r_instr_pq, SEXP r_prices_instrpq_idx, SEXP r_dates,
				  SEXP r_num_fact, SEXP r_q_alloc_mtx, SEXP r_prices_qalloc_idx,
				  SEXP r_prices, SEXP r_positions, SEXP r_positions_p, 
				  SEXP r_sig_mtx, SEXP r_sig_actions, SEXP r_params) {

  SEXP rl=R_NilValue;
  char* exceptionMesg=NULL;

  try {
    Rcpp::CharacterVector instr_p(r_instr_p);
    Rcpp::SimpleVector<INTSXP> tickers_instrp_idx(r_tickers_instrp_idx);
    Rcpp::LogicalVector pca(r_pca);
    bool is_pca = pca[0];
    Rcpp::SimpleVector<INTSXP> num_factors_(r_num_fact);
    int num_factors = num_factors_[0]; 
    // cout << "Num. factors: " << num_factors << endl;
    Rcpp::CharacterVector instr_pq(r_instr_pq);
    Rcpp::SimpleVector<INTSXP> prices_instrpq_idx(r_prices_instrpq_idx);
    Rcpp::CharacterVector dates(r_dates);
    Rcpp::CharacterVector pq_factor_list;
    Rcpp::NumericVector q_alloc_mtx;
    Rcpp::SimpleVector<INTSXP> prices_qalloc_idx;
    map<int,int> qalloc_idx_to_price_col;
    int eigenport_length;
    if(!is_pca){
      pq_factor_list = Rcpp::CharacterVector(r_q_alloc_mtx);
      cout << "PCA NOT in effect" << endl;
    } else {
      q_alloc_mtx = Rcpp::NumericVector(r_q_alloc_mtx);
      prices_qalloc_idx = Rcpp::SimpleVector<INTSXP>(r_prices_qalloc_idx);
      vector<int> dims_buffer(2,0); //hold dimensions by get_dims
      get_dims(q_alloc_mtx, dims_buffer);
      eigenport_length = dims_buffer[1]/num_factors;
      for (int i = 0; i < eigenport_length; ++i){
	qalloc_idx_to_price_col.insert(std::pair<int,int>(i,prices_qalloc_idx(i)-1));
      }
    }
    // map<int,int>::iterator it;
    // // showing contents:
    // std::cout << "qalloc_idx_to_price_col contains:\n";
    // for ( it=qalloc_idx_to_price_col.begin() ; it != qalloc_idx_to_price_col.end(); it++ )
    //   std::cout << (*it).first << " => " << (*it).second << endl;

    Rcpp::NumericVector prices(r_prices);
    Rcpp::SimpleVector<INTSXP> positions(r_positions);
    Rcpp::SimpleVector<INTSXP> positions_p(r_positions_p);
    Rcpp::NumericVector sig_mtx(r_sig_mtx);
    Rcpp::NumericVector sig_actions(r_sig_actions);
    RcppParams params(r_params);
    double init_cash = params.getDoubleValue("init.cash");
    string position_allocation(params.getStringValue("pos.allocation"));
    bool opt_dollar_neutral = false;
    if (position_allocation=="dollar.neutral"){ opt_dollar_neutral = true; cout << "using dollar neutral alloc." << endl; }
    bool opt_silent = params.getBoolValue("silent");
    bool debug = params.getBoolValue("debug");
    string debug_name(params.getStringValue("debug.name")); 
    int debug_j = -1;
    if(debug){
      for (int j = 0; j < instr_p.size(); j++)
	if (string(instr_p(j)) == debug_name){
	  debug_j = j;
	  cout << "Debugging on " << debug_name << endl;
	  break;
	}
    }
    
    RcppResultSet rs;
    vector<int> dims_buffer(2,0); //hold dimensions by get_dims
    get_dims(sig_mtx, dims_buffer);
    const int sig_mtx_2dcols = dims_buffer[1];
    get_dims(sig_actions, dims_buffer);
    const int num_tkrs = dims_buffer[1];
    static const int SIG_COL_OFFSET = sig_mtx_2dcols / num_tkrs;
    enum sig_arr_idx { SA_S_IDX = 1, SA_K_IDX = 2, SA_BETA_IDX = 8 };


    int i,j;
    map<string,int> p_name_to_tkr_idx;
    map<int,int> p_idx_to_tkr_idx;
    map<string,int> pq_name_to_price_col;
    map<int,int> pq_idx_to_price_col;
    // populate maps.  NB: must subtract 1 from indices to convert from R's 1-indexed arrays to C++
    for (int i = 0; i < instr_p.size(); ++i){
      p_name_to_tkr_idx.insert(std::pair<string,int>(string(instr_p(i)),tickers_instrp_idx(i)-1));
      p_idx_to_tkr_idx.insert(std::pair<int,int>(i,tickers_instrp_idx(i)-1));
    }
    for (int i = 0; i < instr_pq.size(); ++i){
      pq_name_to_price_col.insert(std::pair<string,int>(string(instr_pq(i)),prices_instrpq_idx(i)-1));
      pq_idx_to_price_col.insert(std::pair<int,int>(i,prices_instrpq_idx(i)-1));
    }

    bool print_dimensions = false;
    if(print_dimensions){
      Rprintf("instr_p has length %d\n",instr_p.size());
      Rprintf("instr_pq has length %d\n",instr_pq.size());
      Rprintf("dates has length %d\n",dates.size());
      if(!is_pca){
	Rprintf("pq_factor_list has length %d\n",pq_factor_list.size());
	get_dims(pq_factor_list, dims_buffer);
      }
      Rprintf("got dims %d,%d\n",dims_buffer[0],dims_buffer[1]);
      get_dims(prices, dims_buffer);
      //      Rcpp::RObject::AttributeProxy dim_att(prices, "dim");
      //vector<int> pric_dims((string)dim_att);
      //      const vector<int> pric_dims((std::vector <int>)prices.attr("dim"));
      //cout << "prices: directly getting dims: " 
      //	   << pric_dims << endl;
      Rprintf("prices has dims %d,%d\n",dims_buffer[0],dims_buffer[1]);
      get_dims(positions, dims_buffer);
      Rprintf("positions has dims %d,%d\n",dims_buffer[0],dims_buffer[1]);
      get_dims(sig_mtx, dims_buffer);
      Rprintf("sig_mtx has dims %d,%d\n",dims_buffer[0],dims_buffer[1]);
      get_dims(sig_actions, dims_buffer);
      Rprintf("sig_actions has dims %d,%d\n",dims_buffer[0],dims_buffer[1]);

      // cout << "different way to get dimensions: " << endl;
      // vector<int> sa_dims(get_dimensions(sig_actions));
      // Rcpp::RObject y = sig_actions.attr("dim");
      // vector<int> sa_dims(y.asStdVectorInt());
      // Rprintf("sig_actions has dims %d,%d\n",sa_dims[0],sa_dims[1]);

      cout << "third way to get dimensions: " << endl;
      Rcpp::RObject::AttributeProxy sa_dims_ap(sig_actions.attr("dim"));
      vector<int> sa_dims2(Rcpp::as< vector<int> >(sa_dims_ap));
      Rprintf("sig_actions has dims %d,%d\n",sa_dims2[0],sa_dims2[1]);

      // proper way according to romain
      cout << "Proper way to get dimensions: " << endl;
      vector<int> sa_dims3 = sig_actions.attr("dim");
      Rprintf("sig_actions has dims %d,%d\n",sa_dims3[0],sa_dims3[1]);

      int i,j;
      for(i=0;i<1;i++){
	cout << "date: " << string(dates(i)) << endl;
	for(j=0;j<instr_p.size();j++){
	  if(j >= 3){ break; }
	  int tkr_idx = p_name_to_tkr_idx[string(instr_p(j))];
	  cout << string(instr_p(j)) << ":" << tkr_idx << " ";
	  cout << i << "," << j << ":" << get_kth_sigArr_entry(sig_mtx,i,j,SA_S_IDX,SIG_COL_OFFSET) << endl;
	}
	cout << endl;
      }
      cout << endl;
    }



    int num_pq = instr_pq.size();
    vector<int> net_positions(num_pq, 0);
    vector<double> day_prices(num_pq, 0);
    double instr_price;
    double nav = 0;
    double lambda = (double)2/max(100,instr_p.size());
    // double lambda = (double)2/400;
    double cash = init_cash;
    vector<double> equity(dates.size(), 0);
    /* -----------  main trading loop ------------------ */
    bool DONOTRUN=false;
    // for(i=0; i < dates.size(); i++){
    for(i=0; i < 300; i++){
      if(DONOTRUN) { break; }
      
      //      if(!opt_silent){ if(i % 50 == 0) cout << i << " "; }
      if(!opt_silent){ if(i % 50 == 0) Rprintf("%d ",i); }
      col_sums(positions,net_positions);
      
      // cout << "i:" << i << "---------- @pos:" << endl;
      nav = 0;
      for(int ii=0; ii < num_pq; ii++){
	day_prices[ii] = prices(i, pq_idx_to_price_col[ii]);
	if(isnan(day_prices[ii])){ day_prices[ii] = 0; }
	nav += day_prices[ii] * net_positions[ii];
	// cout << ii << ":" << net_positions[ii] << " ";
      }
      
      equity[i] = cash + nav;
      // cout << "cash: " << cash << " equity: " << equity[i] << endl;

      //  for(j=0; j < 3; j++){
      for(j=0; j < instr_p.size(); j++){
	if (debug)
	  if(j!=debug_j) { continue; } else { /*cout << endl;*/ }

	vector<double> eigenport_prices(eigenport_length,0);
	vector<int> num_shr_q(eigenport_length,0);
	vector<double> inv_amount_q(eigenport_length,0);


	int tkr_idx = p_idx_to_tkr_idx[j];
	int current_pos = positions_p(j);

	if(R_IsNA(sig_actions(i,tkr_idx)))
	  continue;
	int actions_code = sig_actions(i,tkr_idx);
	// NB: to check for NA must do the check before converting to int

	string actions_code_str(int_to_binary(sig_actions(i,tkr_idx)));
	bool sig_model_valid = (bool)(actions_code_str[0] - '0');
	bool sig_bto = (bool)(actions_code_str[1] - '0');
	bool sig_sto = (bool)(actions_code_str[2] - '0');
	bool sig_close_short = (bool)(actions_code_str[3] - '0');
	bool sig_close_long = (bool)(actions_code_str[4] - '0');

	double s_score = get_kth_sigArr_entry(sig_mtx,i,tkr_idx,SA_S_IDX,SIG_COL_OFFSET);
	double k_value = get_kth_sigArr_entry(sig_mtx,i,tkr_idx,SA_K_IDX,SIG_COL_OFFSET);
        vector<double> betas(num_factors,0);
	get_kth_sigArr_slice(sig_mtx,i,tkr_idx,SA_BETA_IDX,SIG_COL_OFFSET,num_factors,
			     betas);
	// for(int ii=0; ii < betas.size(); ii++){ cout << "b" << ii << ":" <<
	//     betas[ii] << " "; }
	// cout << endl;

	/*	*/
	bool print_signals = false;
	if(debug && j==debug_j && print_signals){
	  cout << instr_p(j) << ":";
	  cout << "i:" << i << " mv: "  << sig_model_valid << " bto: "  << sig_bto 
	     << " sto: "  << sig_sto << " sig_close_short: "  << sig_close_short << " sig_close_long: " << sig_close_long << endl;
	}
	if(!sig_model_valid){ continue; }


	
	double tot = lambda * equity[i];
	
	int instr_price_idx = pq_name_to_price_col[string(instr_p(j))];
	double this_price = prices(i, instr_price_idx);
	int num_shr_p;
	num_shr_p = cint(tot / this_price); 

	int saw_na_price = 0;
	int MAX_NA_OBS = 6;
	for(int kk=0; kk<eigenport_length; kk++){
	  eigenport_prices[kk] = prices(i, qalloc_idx_to_price_col[kk]);
	  // cout << "ep:" << kk << ":" << qalloc_idx_to_price_col[kk] 
	  //      << ":" << eigenport_prices[kk] << "|";
	  if(isnan(eigenport_prices[kk]) || eigenport_prices[kk] <= 0){
	    saw_na_price++;
	    eigenport_prices[kk]=0;
	  }
	  for(int jj=0; jj<num_factors; jj++){
	    // cout << "accessing index " << kk+jj*eigenport_length << endl;
	    inv_amount_q[kk] += betas[jj] * q_alloc_mtx(i,kk+jj*eigenport_length);
	  }
	  num_shr_q[kk] = -cint(tot * inv_amount_q[kk] / eigenport_prices[kk]);
	  if(isnan(eigenport_prices[kk])){ num_shr_q[kk] = 0; }
	}
	if(isnan(this_price) || (saw_na_price > MAX_NA_OBS) || this_price <= 0)
	  continue; 
	

	if(debug && j==debug_j){
	  double inv_in_p, inv_in_q=0;
	  inv_in_p = tot; 
	  for(int ii=0; ii < eigenport_length; ii++){ inv_in_q += tot*inv_amount_q[ii]; }

	  cout << "i:" << i << "; eq[i]: " << equity[i] << "; cash: " << cash << "; nav: " << nav 
	       << "curr P pos: " << current_pos << " targets: num p " << num_shr_p << " invp " << inv_in_p << " invq " << inv_in_q << endl; 
	  cout  << "portf. Q's and b's: ";
	  for(int ii=0; ii < betas.size(); ii++){ cout << "b" << ii << ":" <<
	      betas[ii] << " Q" << ii << ":" << inv_amount_q[ii]; }
	  cout << endl;
	}

	if(sig_sto && (current_pos >= 0)){ //flat or long
	  //           	sell stock, buy factors #opening short (if flat before, as we should be)
	  //            num.shrs has the correct signs for long, this is short though
	  num_shr_p = -num_shr_p;
	  for(int kk=0; kk<eigenport_length; kk++){
	    num_shr_q[kk] = -num_shr_q[kk];
	    positions(j,qalloc_idx_to_price_col[kk]) += num_shr_q[kk];
	    cash -= num_shr_q[kk] * eigenport_prices[kk];
	  }
	  positions(j, instr_price_idx) += num_shr_p;
	  positions_p(j) += num_shr_p;
	  cash -= this_price*num_shr_p;
	  if(debug && j==debug_j){ cout << i << ": STO on " << instr_p(j) << endl; 
	    double q_inv=0; double p_inv = num_shr_p * this_price;
	    for(int kk=0; kk<eigenport_length; kk++){
	      q_inv += num_shr_q[kk] * eigenport_prices[kk];
	      if(num_shr_q[kk]>0){ cout << kk << ":" << num_shr_q[kk] << "|"; }
	    } 
	    cout << endl;
	    cout << i << ": STO on " << instr_p(j) 
		 << " P inv: " << p_inv << " Q inv: " 
		 << q_inv << " eff.beta: " << (double)abs(p_inv)/abs(q_inv) << endl;
	  }
	} //#else do nothing #already short 
	if(sig_close_short && (current_pos < 0)){
	  //           ## buy stock, sell factors #closing short
	  cash += this_price*positions(j, instr_price_idx);
	  for(int kk=0; kk<eigenport_length; kk++){
	    cash += positions(j,qalloc_idx_to_price_col[kk]) * eigenport_prices[kk];
	    positions(j,qalloc_idx_to_price_col[kk]) = 0;
	  }
	  positions(j, instr_price_idx) = 0;
	  positions_p(j) = 0;
	  if(debug && j==debug_j){ cout << i << ": CS on " << instr_p(j) << endl; }
	}//#else: do nothing
	if(sig_bto && (current_pos <= 0)){ // flat or (anomalously) short
	  //           ##        buy stock, sell factors #opening long
	  positions(j, instr_price_idx) += num_shr_p;
	  positions_p(j) += num_shr_p;
	  for(int kk=0; kk<eigenport_length; kk++){
	    positions(j,qalloc_idx_to_price_col[kk]) += num_shr_q[kk];
	    cash -= num_shr_q[kk] * eigenport_prices[kk];
	    //	    if(num_shr_q[kk] > 0){ cout << kk << ":" << num_shr_q[kk] << "|"; }
	  }
	  cash -= this_price*num_shr_p;

	  if(debug && j==debug_j){ cout << i << ": BTO on " << instr_p(j) << endl; 
	    double q_inv=0; double p_inv = num_shr_p * this_price;
	    for(int kk=0; kk<eigenport_length; kk++){
	      q_inv += num_shr_q[kk] * eigenport_prices[kk];
	    }
	    cout << i << ": BTO on " << instr_p(j) 
		 << " P inv: " << p_inv << " Q inv: " 
		 << q_inv << " eff.beta: " << (double)abs(p_inv)/abs(q_inv) << endl;
	  }
	}//# else: do nothing #already long
	if(sig_close_long && (current_pos > 0)){
	  //           ##          sell stock, buy factors #closing long
	  if(debug && j==debug_j){ cout << i << ": CL on " << instr_p(j) << endl; }
	  cash += this_price*positions(j, instr_price_idx);
	  for(int kk=0; kk<eigenport_length; kk++){
	    cash += positions(j,qalloc_idx_to_price_col[kk]) * eigenport_prices[kk];
	    positions(j,qalloc_idx_to_price_col[kk]) = 0;
	  }
	  positions(j, instr_price_idx) = 0;
	  positions_p(j) = 0;
          } 
   }
 }

      /* -- end main trading loop -- */

    rs.add("cash",cash);
    rs.add("nav",nav);
    rs.add("equity",equity);

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
    double n1 = Rcpp::as<double>(n1sexp);
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

