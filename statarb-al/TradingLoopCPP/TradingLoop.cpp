#include <Rcpp.h>
#include <Rcpp/RObject.h>
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <sstream>
#include <cstring>

template<class T>
std::string toString(const T& t){
  std::ostringstream stream;
  stream << t;
  return stream.str();
}

template<class T>
T fromString(const std::string& s){
  std::istringstream stream (s);
  T t;
  stream >> t;
  return t;
}

void get_dims(Rcpp::VectorBase &vec, std::vector<int> &dims){
    Rcpp::RObject y = vec.attr("dim");
    std::vector<int> tmp = y.asStdVectorInt();
    for(int i=0; i<tmp.size(); i++){
      dims[i] = tmp[i];
    }
}

RcppExport SEXP backtest_loop(SEXP r_instr_p, SEXP r_instr_q, SEXP r_dates,
			      SEXP r_pq_factor_list, SEXP r_prices, SEXP r_positions, 
			      SEXP r_sig_mtx, SEXP r_sig_actions, SEXP r_params) {

  SEXP rl=R_NilValue;
  char* exceptionMesg=NULL;
  
  try {
    Rcpp::CharacterVector instr_p(r_instr_p);
    Rcpp::CharacterVector instr_q(r_instr_q);
    Rcpp::CharacterVector dates(r_dates);
    Rcpp::CharacterVector pq_factor_list(r_pq_factor_list);
    Rcpp::NumericVector prices(r_prices);
    Rcpp::NumericVector positions(r_positions);
    Rcpp::NumericVector sig_mtx(r_sig_mtx);
    Rcpp::NumericVector sig_actions(r_sig_actions);
    // RcppParams params(r_params);
    RcppResultSet rs;
    
    std::vector<int> mtx_dims (2,0);

    Rprintf("instr_p has length %d\n",instr_p.size());
    Rprintf("instr_q has length %d\n",instr_q.size());
    Rprintf("dates has length %d\n",dates.size());
    Rprintf("pq_factor_list has length %d\n",pq_factor_list.size());
    get_dims(pq_factor_list, mtx_dims);
    Rprintf("got dims %d,%d\n",mtx_dims[0],mtx_dims[1]);
    get_dims(prices, mtx_dims);
    Rprintf("prices has dims %d,%d\n",mtx_dims[0],mtx_dims[1]);
    get_dims(positions, mtx_dims);
    Rprintf("positions has dims %d,%d\n",mtx_dims[0],mtx_dims[1]);
    get_dims(sig_mtx, mtx_dims);
    Rprintf("sig_mtx has dims %d,%d\n",mtx_dims[0],mtx_dims[1]);
    get_dims(sig_actions, mtx_dims);
    Rprintf("sig_actions has dims %d,%d\n",mtx_dims[0],mtx_dims[1]);



       // Rprintf("pq_factor_list has length %d,%d\n",pq_factor_list.rows(),pq_factor_list.cols());
    int i,j;
    i=j=0;
    std::string prefix("test");
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

    std::vector<double> v1_stl(v1.stlVector());
    if(R_IsNA(v1_stl[0]))
      Rprintf("Found NA in STL vector!\n");

    double m00 = m1(0,0);
    Rprintf("00 element of the matrix is %f\n",m00);
    if(R_IsNA(m00))
      Rprintf("Found NA in matrix!\n");
    else
      Rprintf("did not detect NA in matrix!\n");
    
    Rprintf("printing STL matrix:\n");
    std::vector< std::vector<double> > m1_stl(m1.stlMatrix());
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

