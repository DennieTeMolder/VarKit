#include <zlib.h>
#include <string>
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

#define LENGTH 4000 // hexadecimal for 16.384 KB

void strsplit(std::string& str, char& split, std::vector<std::string>& result){
  int start = 0;
  unsigned int i = 0;

  for(i = 1; i < str.size(); i++){
    if(str[i] == split){
      std::string temp = str.substr(start, i - start);
      result.push_back(temp);
      start = i+1;
    }
  }

  // Handle the last element.
  std::string temp = str.substr(start, i - start);
  result.push_back(temp);
}

std::string next_chunk(gzFile file, int nchar) {
  int bytes_read;
  char buffer[nchar];

  bytes_read = gzread(file, buffer, nchar - 1);
  buffer[bytes_read] = '\0';

  return reinterpret_cast<char*>(buffer);
}

// [[Rcpp::export]]
CharacterVector index_vcfC(std::string x) {

  gzFile file = gzopen(x.c_str(), "r");

  if (! file) {
    Rcpp::stop("gzopen of " + x + " failed: " + strerror(errno));
  }

  int err;
  std::string str = next_chunk(file, LENGTH);

  char split = '\n'; // Must be single quotes!
  std::vector<std::string> lines;
  strsplit(str, split, lines);

  gzclose(file);

  return wrap(lines);
}
