#include <Rcpp.h>
#include <map>
using namespace Rcpp;
using namespace std;

// Variables
// Input dataframe
DataFrame Text_;
// Wanted group names
String Group1;
String Group2;
// List of words to calculate JSD for
CharacterVector WordList;
// String variables with input column names
String Group_;
String Word_;
String Count_;
// Dataframe to hold word probabilities
map<String, pair<double, double>> wordProbs;
// Map to hold final results
DataFrame results_;

// Functions
// Jensen-Shannon Divergence
double calcJSD(double, double);
// Kullback-Leibler Divergence
double kld(double, double);
// Joint Entropy
double h(double, double);
// Entropy
double h(double);
// Calculate word probabilites
void getProbabilites();
// Get JSD of one group pairing
void jsdPair();
// Initialize wordProbs
void setWordProbs();
// Clear private variables
void clearVars();

// Exported function
//' JSD
//' @description Calculates the JSD score for each word between group pairings. To use this function, the user must provide a data frame with a column for words, a column for the text group, and a column for the count of the word in that group. The default column names are "word", "group", and "n", but these can be changed using the parameters word, group, and n. The default settings will calculate the JSD for all words between the first two groups in the data set. However, the user can provide a list of words using the word_list parameter and/or a list of groups using the group_list parameter. If more than two groups are given, the function will provide the JSD scores all all pairs of groups.
//' @param text Data frame containing data
//' @param group_list Vector containing all groups to find pairwise JSD scores for
//' @param word_list Vector containing all words to find JSD scores for
//' @param group Name of data frame column containing text group
//' @param word Name of data frame column containing words
//' @param n Name of data frame column containing word count in text group
//' @return Data frame containing a column containing unique words and columns for JSD scores for each group pair
//' @examples
//' # Load example Hansard 1820 dataset
//' data(hansard_1820_example)
//' head(hansard_1820_example)
//'
//' # Calculate JSD for given words and groups
//' output = jsd(
//'   hansard_1820_example,
//'   group = "speaker",
//'   group_list = c("Mr. Hume", "Mr. Brougham"),
//'   word_list = c("house", "person")
//' )
//' head(output)
//' @useDynLib dhmeasures
//' @importFrom Rcpp evalCpp
//' @exportPattern ^[[:alpha:]]+
// [[Rcpp::export]]
DataFrame jsd(DataFrame text, CharacterVector group_list = CharacterVector::create(), CharacterVector word_list = CharacterVector::create(), String group = "group", String word = "word", String n = "n") {
  // Set private variables to input values
  Text_ = text;
  Group_ = group;
  Word_ = word;
  Count_ = n;
  if (word_list.size() == 0) {
    // If word_list is empty, use all words
    CharacterVector allWords = text[word];
    WordList = unique(allWords);
  } else {
    // Else, use given words
    WordList = word_list;
  }

  // Initialize wordProbs
  setWordProbs();

  // If not enough groups given, pick for user
  if (group_list.size() < 2) {
    // Get all unique groups
    CharacterVector allGroups = Text_[Group_];
    allGroups = unique(allGroups);
    if (group_list.size() == 1) {
      // If 1 group was given, set Group1 to given group
      Group1 = group_list[0];
      if (allGroups[0] == Group1) {
        // If first group is Group1, set Group2 to second group
        Group2 = allGroups[1];
      } else {
        // Else, set Group2 to first group
        Group2 = allGroups[0];
      }
    } else {
      // Else, use first two groups
      Group1 = allGroups[0];
      Group2 = allGroups[1];
    }

    // Return results of JSD for selected pair
    jsdPair();
    // Clear private variables
    clearVars();

    // Return results
    return results_;
  }

  // Initialze output data frame
  DataFrame output = DataFrame::create();
  // Loop through given groups
  for (int i = 0; i < group_list.size(); i++) {
    // Loop through groups after current group
    for (int j = i + 1; j < group_list.size(); j++) {
      String g1 = group_list[i];
      String g2 = group_list[j];
      /// Set groups 1 and 2 to current groups
      Group1 = group_list[i];
      Group2 = group_list[j];
      // Calculate JSD scores for group pairing
      jsdPair();
      // Store JSD scores
      CharacterVector names = results_.names();
      String name = names[1];
      output.push_back(results_[1], name);

    }
  }
  // Add word column to output
  output.push_front(results_["word"], "word");

  // Clear private variables
  clearVars();

  return output;
}

// Jensen-Shannon Divergence
double calcJSD(double p, double q) {
  // If p = 0 or q = 0, return 0
  if (p == 0.0 || q == 0.0) {
    return 0;
  }
  // Find midpoint
  double r = (p + q) / 2;
  // Calculate JSD
  return (kld(p, r) + kld(q, r)) / 2;
}

// Kullback-Leibler Divergence
double kld(double p, double q) {
   // Return joint entropy - entropy(P)
   double val = h(p, q) - h(p);
   return val;
}

// Joint Entropy
double h(double x, double y) {
  // Return joint probability * log(joint robability)
  double jointProb = x * y;
  return jointProb * log(jointProb);
}

// Entropy
double h(double x) {
  // Return probability * log(probability)
  return x * log(x);
}

// Calculate word probabilites
void getProbabilites() {
  // Initialize ints for group counts
  int count1 = 0;
  int count2 = 0;

  // Get vectors for group, word, and count
  CharacterVector allGroups = Text_[Group_];
  CharacterVector allWords = Text_[Word_];
  NumericVector allCounts = Text_[Count_];

  // Loop through Text_ dataframe
  for (int i = 0; i < Text_.nrows(); i++) {
    // Only include word in WordList
    if (find(WordList.begin(), WordList.end(), allWords[i]) != WordList.end()) {
      // Check if row group is either Group1 or Group2
      if (allGroups[i] == Group1) {
        // Set word count in group
        wordProbs[allWords[i]].first = allCounts[i];
      } else if (allGroups[i] == Group2) {
        // Set word count in group
        wordProbs[allWords[i]].second = allCounts[i];
      }
    }
    // Update group counts
    if (allGroups[i] == Group1) {
      // Update group 1 count
      count1 += allCounts[i];
    } else if (allGroups[i] == Group2) {
      // Update group 2 count
      count2 += allCounts[i];
    }
  }

  // Loop through wordProbs
  for (auto& itr : wordProbs) {
    // Divide each count by total count for that group
    itr.second.first /= count1;
    itr.second.second /= count2;
  }
}

// Calculate the JSD score for a group pairing
void jsdPair() {
  // Calculate word probabilities
  getProbabilites();

  // Initialize vectors
  CharacterVector words;
  NumericVector scores;

  // Loop through wordProbs
  for (const auto& itr : wordProbs) {
      // If both probabilities are nonzero, get JSD of current word
      double jsd = calcJSD(itr.second.first, itr.second.second);
      // Append word and jsd to vectors
      words.push_back(itr.first);
      scores.push_back(jsd);
  }

  // Use concatenated group names as column name
  String name = Group1;
  name += "_";
  name += Group2;

  // Create results dataframe from vectors
  results_ = DataFrame::create(Named(Word_) = words, Named(name) = scores);
}

// Initialize wordProbs
void setWordProbs() {
  // Loop through WordList
  for (int i = 0; i < WordList.size(); i++) {
    // Add pair to wordProbs for current word
    pair<double, double> temp;
    wordProbs[WordList[i]] = temp;
  }
}

// Clear private variables
void clearVars() {
  Text_ = DataFrame::create();
  Group1 = "";
  Group2 = "";
  WordList = CharacterVector::create();
  Group_ = "";
  Word_ = "";
  Count_ = "";
  wordProbs.clear();
}
