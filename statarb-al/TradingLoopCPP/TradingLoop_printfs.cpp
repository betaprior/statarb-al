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
    vector<int> tmp = y.asStdVectorInt();
    for(int i=0; i<tmp.size(); i++){
      dims[i] = tmp[i];
    }
}

template <typename T, int RTYPE, typename CTYPE>
void col_sums(Rcpp::SimpleVector<RTYPE, CTYPE> &vec, vector<T> &result){
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
    Rcpp::SimpleVector<INTSXP,int> positions(r_positions);
    Rcpp::NumericVector sig_mtx(r_sig_mtx);
    Rcpp::NumericVector sig_actions(r_sig_actions);
    RcppParams params(r_params);
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
    static const int SIG_COL_OFFSET = sig_mtx_2dcols / instr_p.size();
    enum sig_arr_idx { SA_S_IDX = 1, SA_K_IDX = 2, SA_BETA_IDX = 8 };


    int i,j;
    map<string,int> p_name_to_tkr_idx;
    map<string,int> pq_name_to_price_col;
    // populate maps.  NB: must subtract 1 from indices to convert from R's 1-indexed arrays to C++
    for (int i = 0; i < instr_p.size(); ++i)
      p_name_to_tkr_idx.insert(std::pair<string,int>(string(instr_p(i)),tickers_instrp_idx(i)-1));
    for (int i = 0; i < instr_pq.size(); ++i)
      pq_name_to_price_col.insert(std::pair<string,int>(string(instr_pq(i)),prices_instrpq_idx(i)-1));

    

    bool print_dimensions = false;
    if(print_dimensions){
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
    }
    //    map<string,int>::iterator it;
    // showing contents:
    //    std::cout << "pq_name_to_price_col contains:\n";
    // for ( it=pq_name_to_price_col.begin() ; it != pq_name_to_price_col.end(); it++ )
    //   std::cout << (*it).first << " => " << (*it).second << endl;
    int num_pq = instr_pq.size();
    vector<int> net_positions(num_pq, 0);
    vector<double> day_prices(num_pq, 0);
    double instr_price;
    double nav = 0;
    double lambda = (double)2/max(100,instr_p.size());
    double cash = 100000;
    vector<double> equity(dates.size(), 0);
    /* -----------  main trading loop ------------------ */
       for(i=0; i < dates.size(); i++){
    //        for(i=0; i < 55; i++){
      //      cout << endl << "date: " << dates(i) << endl;
      //      cout << endl << i << ":" << endl;
      
      if(!opt_silent){ if(i % 50 == 0) cout << i << " "; }
      col_sums(positions,net_positions);
      
      nav = 0;
      for(int ii=0; ii < num_pq; ii++){
	day_prices[ii] = prices(i, pq_name_to_price_col[string(instr_pq(ii))]);
	if(isnan(day_prices[ii])){ day_prices[ii] = 0; }
	nav += day_prices[ii] * net_positions[ii];
      }
      
      equity[i] = cash + nav;

      for(j=0; j < instr_p.size(); j++){
	if (debug)
	  if(j!=debug_j) { continue; }else { /*cout << endl;*/ }
	// cout << j << "-";
      //  for(j=0; j < 3; j++){
	int instr_price_idx = pq_name_to_price_col[string(pq_factor_list(j,0))];
	int pair_price_idx = pq_name_to_price_col[string(pq_factor_list(j,1))];
	double pair_price = prices(i, pair_price_idx);
	double this_price = prices(i, instr_price_idx);
	if((isnan(this_price) || isnan(pair_price)) || (this_price*pair_price <= 0))
	  continue;
	int tkr_idx = p_name_to_tkr_idx[string(instr_p(j))];
	int current_pos = positions(j,instr_price_idx);

	if(R_IsNA(sig_actions(i,j)))
	  continue;
	int actions_code = sig_actions(i,j);
	// NB: to check for NA must do the check before converting to int

  	// cout << "this instr: " << instr_p(j);
  	// cout << "actions string: " << int_to_binary(sig_actions(i,j)) << endl;
  	// cout << "s-score: " << get_kth_sigArr_entry(sig_mtx,i,j,SA_S_IDX,SIG_COL_OFFSET) << endl;

	string actions_code_str(int_to_binary(sig_actions(i,j)));
	bool sig_model_valid = (bool)(actions_code_str[0] - '0');
	bool sig_bto = (bool)(actions_code_str[1] - '0');
	bool sig_sto = (bool)(actions_code_str[2] - '0');
	bool sig_close_short = (bool)(actions_code_str[3] - '0');
	bool sig_close_long = (bool)(actions_code_str[4] - '0');

	double s_score = get_kth_sigArr_entry(sig_mtx,i,j,SA_S_IDX,SIG_COL_OFFSET);
	double k_value = get_kth_sigArr_entry(sig_mtx,i,j,SA_K_IDX,SIG_COL_OFFSET);
        double beta    = get_kth_sigArr_entry(sig_mtx,i,j,SA_BETA_IDX,SIG_COL_OFFSET);
	/*	*/
	bool print_signals = false;
	if(debug && j==debug_j && print_signals){
	  cout << instr_p(j) << ":";
	  cout << "i:" << i << " mv: "  << sig_model_valid << " bto: "  << sig_bto 
	     << " sto: "  << sig_sto << " sig_close_short: "  << sig_close_short << " sig_close_long: " << sig_close_long << endl;
	}
	if(!sig_model_valid){ continue; }
  //       if(debug && this.name==debug.name) cat(i,"pos:",this.p,"inv.targ:",tot,"beta ",betas," prices: ",price.s.b," num shares: ",num.shrs,"INVALID\n",file=outfile,append=TRUE)
  //     }else{
	
	double tot = lambda * equity[i];
	int num_shr_p = cint(tot / this_price); 
	int num_shr_q = -cint(tot*beta / pair_price);

	
	if(debug && j==debug_j){
	  cout << "i:" << i << "; eq[i]: " << equity[i] << "; cash: " << cash << "; nav: " << nav 
	       << "curr P pos: " << current_pos << " targets: num p " << num_shr_p << " num q " << num_shr_q << endl; 
	  cout  << "tot is " << tot << " beta is " << beta  << " this_price is " 
		<< this_price << " pair_pr " << pair_price << endl; }

	if(sig_sto){
	  if(current_pos >= 0){ //flat or long
  //           ##	sell stock, buy factors #opening short (if flat before, as we should
  //           ##be)
  //           ## num.shrs has the correct signs for long, this is short though
	    num_shr_p = -num_shr_p;
	    num_shr_q = -num_shr_q;
	    positions(j, instr_price_idx) += num_shr_p;
	    positions(j, pair_price_idx) += num_shr_q;
	    cash -= this_price*num_shr_p + pair_price*num_shr_q;
  //           if((debug && this.name==debug.name)||dbg.transactions)
  //             cat(i,this.name,"STO: 'acquiring'",num.shrs,"paying ",sum(price.s.b * num.shrs),"cash=",cash,"\n")
  //           if(this.p>0 && warn) { cat(paste("\nSTO tripped while long, on day",i,"for stock",this.name),"\n"); if(stop.on.wrn) stop() }
	    if(debug && j==debug_j){ cout << i << ": sTO on " << instr_p(j) << endl; }
		    
          }
	} //#else do nothing #already short 
	if(sig_close_short){
	  if(current_pos < 0){
	    //           ## buy stock, sell factors #closing short
	    cash += this_price*positions(j, instr_price_idx) + pair_price*positions(j, pair_price_idx);
	    //           if((debug && this.name==debug.name)||dbg.transactions)
	    //             cat(i,this.name,"CLOSING SHORT: paying ",-sum(price.s.b*c(positions[j,this.name],positions[j,pair.name])),"cash=",cash,"\n")
	    if(debug && j==debug_j){ cout << i << ": CS on " << instr_p(j) << endl; }

	    positions(j, instr_price_idx) = 0;
	    positions(j, pair_price_idx) = 0;
          }
        }//#else: do nothing
	if(sig_bto){
	  if(current_pos <= 0){ // flat or (anomalously) short
	    //           ##        buy stock, sell factors #opening long
	    positions(j, instr_price_idx) += num_shr_p;
	    positions(j, pair_price_idx) += num_shr_q;
	    cash -= this_price*num_shr_p + pair_price*num_shr_q;
	    //           if(this.p<0 && warn){ cat(paste("\nBTO tripped while short, on day",i,"for stock",this.name,"\n")); if(stop.on.wrn) stop() }
	    //           if((debug && this.name==debug.name)||dbg.transactions)
	    //             cat(i,this.name,"BTO: 'acquiring'",num.shrs," paying ",sum(price.s.b * num.shrs),"cash=",cash,"\n")
	    if(debug && j==debug_j){ cout << i << ": BTO on " << instr_p(j) << endl; }
          }//# else: do nothing #already long
	}
	if(sig_close_long){
	  if(current_pos > 0){
	    //           ##          sell stock, buy factors #closing long
	    if(debug && j==debug_j){ cout << i << ": CL on " << instr_p(j) << endl; }

	    cash += this_price*positions(j, instr_price_idx) + pair_price*positions(j, pair_price_idx);
	    positions(j, instr_price_idx) = 0;
	    positions(j, pair_price_idx) = 0;
          } }
	/*		*/
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

