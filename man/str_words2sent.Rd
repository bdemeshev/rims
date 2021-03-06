% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/rims.R
\name{str_words2sent}
\alias{str_words2sent}
\title{Convert data.frame of separate words to a vector of sentences}
\usage{
str_words2sent(d, sep = " ")
}
\arguments{
\item{sep}{the separator for words, space by default}

\item{x}{the data.frame with 3 variables: word, word_n (number of word), sent_n (number of sentence)}
}
\value{
a vector of sentences
#
}
\description{
Convert data.frame of separate words to a vector of sentences
}
\details{
Convert data.frame with 3 variables:
word, word_n (number of word), sent_n (number of sentence)
To a vector of sentences
}
\examples{
str_words2sent(str_sent2words(c("привет","Маша, это я, Дубровский")))
}

