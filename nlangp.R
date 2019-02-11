# Natural language processing
# Load libraries
library(NLP)
library(openNLP)
library(RWeka)

# use the readLines function to read each line as separate character vector
bio <- readLines("tonywee.txt")
print(bio)

# use the paste function to combine all character vectors to a single character vector
bio <- paste(bio, collapse = "tonywee.txt")
print(bio)

# Turn it into words and sentences
library(magrittr)
# use the string function to convert our variable to a string
bio <- as.String(bio)

#Create annotators for word sentences 
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()

#Apply the annotator functions using the annotae function
bio_annotations <- annotate(bio, list(sent_ann, word_ann))

#Looking at the first few items contained in the object
class(bio_annotations)
head(bio_annotations)

#Combine the essay and the annotations to create an AnnotationPlainTextDocument
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)

#Extract info from our document using sents function
sents(bio_doc) %>% head(2)
words(bio_doc) %>% head(10)

#Annotating people and places
#openNLP will find people, places and organizations.
#These annotator functions are created using similar constructor functions like word and sent anne
person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

#create a new pipeline list to hold our annotators in the order we want to apply them, then apply it to the bio variable.
pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)
bio_annotations <- annotate(bio, pipeline)
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)

# Function to extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

# Now we can extract all of the named entities :)
entities(bio_doc, kind = "person")
entities(bio_doc, kind = "location")
entities(bio_doc, kind = "organization")
