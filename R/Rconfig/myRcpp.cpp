#include <fstream>
#include <sstream>
#include <string>
#include <Rcpp.h>
using namespace Rcpp;

// -----------------------------------------------------------------------------
/*
- readRcpp(datafile):

fast reading datafile
Ref: https://gist.github.com/hadley/6353939
*/
// [[Rcpp::export]]
CharacterVector readRcpp(std::string path) {
  std::ifstream in(path.c_str());
  std::string contents;
  in.seekg(0, std::ios::end);
  contents.resize(in.tellg());
  in.seekg(0, std::ios::beg);
  in.read(&contents[0], contents.size());
  in.close();
  return(contents);
}
// -----------------------------------------------------------------------------
