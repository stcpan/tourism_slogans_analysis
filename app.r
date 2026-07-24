# Tourism Slogans Analysis - Shiny App
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(tidyverse)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(wordcloud)
library(RColorBrewer)
library(gridExtra)
library(shinycssloaders)
library(readxl)
library(rnaturalearth) # For geographic data
library(sf) # For spatial data handling
library(stringr) # For string operations
library(cluster)

# Improved function word list (add more as needed)
function_words <- c(
  # Articles
  "the", "a", "an",

  # Prepositions
  "of", "to", "in", "for", "with", "on", "at", "by", "from", "about", "into",
  "through", "during", "before", "after", "above", "below", "up", "down", "out",
  "off", "over", "under", "again", "further", "then", "once", "here", "there",
  "when", "where", "why", "how", "all", "any", "both", "each", "few", "more",
  "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same",
  "than", "too", "very", "can", "will", "just", "should", "now",

  # Pronouns
  "i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your",
  "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she",
  "her", "hers", "herself", "it", "its", "itself", "they", "them", "their",
  "theirs", "themselves", "what", "which", "who", "whom", "this", "that",
  "these", "those", "am", "is", "are", "was", "were", "be", "been", "being",
  "have", "has", "had", "do", "does", "did", "will", "would", "could", "should",
  "may", "might", "must", "shall",

  # Conjunctions
  "and", "or", "but", "if", "while", "because", "so", "as", "until", "while",

  # Common adverbs that shouldn't count for alliteration
  "also", "already", "always", "often", "still", "well", "perhaps", "maybe",
  "really", "actually", "certainly", "probably", "definitely", "especially",

  # Additional function words commonly found in tourism slogans
  "truly", "simply", "just", "beyond", "more", "most", "every", "everything",
  "one", "two", "three", "first", "second", "last", "next", "new", "old",

  # Words that often appear in tourism contexts but are function words
  "all", "much", "many", "little", "less", "best", "better", "good", "great",
  "small", "big", "large", "long", "short", "high", "low", "right", "left"
)

detect_rhyming <- function(text) {
  if (is.na(text) || text == "") {
    return(FALSE)
  }
  words <- tolower(trimws(unlist(strsplit(
    text, "[[:space:]]+"
  ))))
  words <- gsub("[^a-z]", "", words)
  words <- words[nchar(words) > 2]
  # Remove function words
  content_words <- words[!words %in% function_words]
  if (length(content_words) < 2) {
    return(FALSE)
  }
  last_two <- tail(content_words, 2)
  get_end <- function(w) {
    n <- nchar(w)
    if (n >= 3) {
      substr(w, n - 2, n)
    } else {
      w
    }
  }
  end1 <- get_end(last_two[1])
  end2 <- get_end(last_two[2])
  if (last_two[1] == last_two[2]) {
    return(FALSE)
  }
  return(end1 == end2)
}

detect_alliteration <- function(text) {
  if (is.na(text) || text == "") {
    return(FALSE)
  }

  # Clean and split text into words
  words <- tolower(trimws(unlist(strsplit(text, "[[:space:]]+"))))
  # Remove punctuation and keep only alphabetic characters
  words <- gsub("[^a-z]", "", words)
  # Remove very short words (less than 3 characters) and empty strings
  words <- words[nchar(words) >= 3]

  # Remove function words - this is the key improvement
  content_words <- words[!words %in% function_words]

  # Need at least 2 content words for alliteration
  if (length(content_words) < 2) {
    return(FALSE)
  }

  # Get initial consonants/consonant clusters
  initials <- get_initial_sound(content_words)

  # Check if any initial sound occurs at least twice
  return(any(table(initials) >= 2))
}

# Helper function to get initial consonant sound (improved)
get_initial_sound <- function(words) {
  sapply(words, function(word) {
    if (nchar(word) == 0) {
      return("")
    }

    # Handle common consonant clusters at the beginning of words
    if (grepl("^(ch|sh|th|ph|wh|st|sp|sc|sk|sl|sm|sn|sw|tr|pr|br|cr|dr|fr|gr)", word)) {
      # Extract the consonant cluster
      cluster <- regmatches(word, regexpr("^(ch|sh|th|ph|wh|st|sp|sc|sk|sl|sm|sn|sw|tr|pr|br|cr|dr|fr|gr)", word))
      return(cluster)
    } else {
      # Return just the first character for single consonants
      return(substr(word, 1, 1))
    }
  })
}

detect_simile <- function(text) {
  if (is.na(text) || text == "") {
    return(FALSE)
  }

  # Convert to lowercase for pattern matching
  text_lower <- tolower(text)

  # Define simile patterns - explicit comparative structures
  simile_patterns <- c(
    # Basic simile structures
    "\\blike\\s+[a-z]+", # "like paradise", "like heaven"
    "\\bas\\s+[a-z]+\\s+as\\b", # "as beautiful as", "as pure as"
    "\\bsimilar\\s+to\\b", # "similar to"

    # Common tourism simile phrases
    "\\blike\\s+(a\\s+)?(paradise|heaven|dream|jewel|gem|pearl|treasure|oasis|haven)\\b",
    "\\bas\\s+(beautiful|pure|clear|blue|green|white|pristine|perfect|magical|amazing)\\s+as\\b",
    "\\blike\\s+(no\\s+)?(other|nowhere\\s+else)\\b", # "like no other"

    # Metaphorical comparisons that function as similes
    "\\bfeels?\\s+like\\b", # "feels like home", "feel like paradise"
    "\\blooks?\\s+like\\b", # "looks like paradise"
    "\\bsounds?\\s+like\\b", # "sounds like music"
    "\\btastes?\\s+like\\b", # "tastes like heaven"

    # Extended simile patterns
    "\\bmore\\s+[a-z]+\\s+than\\b", # "more beautiful than"
    "\\bjust\\s+like\\b", # "just like"
    "\\bexactly\\s+like\\b", # "exactly like"
    "\\bmuch\\s+like\\b", # "much like"
    "\\bnothing\\s+like\\b" # "nothing like"
  )

  # Check for any simile pattern
  has_simile <- any(sapply(simile_patterns, function(pattern) {
    grepl(pattern, text_lower, perl = TRUE)
  }))

  return(has_simile)
}

detect_personification <- function(text) {
  if (is.na(text) || text == "") {
    return(FALSE)
  }

  # Convert to lowercase for pattern matching
  text_lower <- tolower(text)

  # EXPANDED human action verbs (removed "experience" as noted)
  human_action_verbs <- c(
    "welcomes?", "embrace[ds]?", "invites?", "calls?", "whispers?", "speaks?",
    "tells?", "shares?", "offers?", "gives?", "breathes?", "lives?", "sleeps?",
    "awakens?", "dreams?", "smiles?", "laughs?", "sings?", "dances?",
    "celebrates?", "awaits?", "feels?", "loves?", "kisses", "hugs?",
    "watches?", "sees?", "hears?", "touches?", "holds?", "reaches?",
    "opens?", "closes?", "reveals?", "shows?", "displays?", "expresses?",
    "radiates?", "glows?", "shines?", "sparkles?", "twinkles?", "winks?",
    "beats?", "pulses?", "throbs?", "yearns?", "longs?", "desires?",
    "remembers?", "forgets?", "thinks?", "knows?", "understands?",
    "surprises?", "delights?", "charms?", "enchants?", "captivates?",
    "seduces?", "tempts?", "lures?", "attracts?", "draws?", "pulls?",
    "pushes?", "guides?", "leads?", "follows?", "accompanies?",
    "protects?", "guards?", "shelters?", "nurtures?", "cares?",
    "rewards?", "blesses?", "favors?", "spoils?", "pampers?",
    "inspires?", "motivates?", "encourages?", "supports?", "helps?"
  )

  # EXPANDED non-human subjects (including possessive forms)
  non_human_subjects <- c(
    "nature", "mountains?", "forests?", "oceans?", "seas?", "cities?",
    "islands?", "valleys?", "rivers?", "beaches?", "landscapes?",
    "destination", "paradise", "country", "nation", "land", "place",
    "sun", "moon", "stars?", "wind", "waves?", "trees?", "flowers?",
    "beauty", "life", "warmth", "culture", "tradition", "heritage",
    "adventure", "journey", "spirit", "soul", "heart",
    "coast", "shore", "harbor", "bay", "lake", "stream", "waterfall",
    "desert", "jungle", "wildlife", "fauna", "flora", "ecosystem",
    "architecture", "buildings?", "structures?", "monuments?",
    "history", "past", "future", "time", "moments?", "memories?",
    "magic", "wonder", "mystery", "secrets?", "treasures?",
    "colors?", "sounds?", "scents?", "flavors?", "textures?",
    # Add specific countries/places that are commonly personified
    "south africa", "bolivia", "kenya", "brazil", "thailand", "italy",
    "france", "spain", "greece", "japan", "india", "egypt", "morocco",
    "peru", "chile", "argentina", "australia", "new zealand",
    # Add possessive forms
    "our"
  )

  # PATTERN 1: Standard subject + verb personification
  # Detects both with and without direct objects
  pattern1 <- paste0(
    "\\b(", paste(non_human_subjects, collapse = "|"), ")\\s+",
    "(", paste(human_action_verbs, collapse = "|"), ")\\b"
  )

  # PATTERN 2: "Feel [Country/Place]" constructions (kept as is)
  pattern2 <- "\\bfeel\\s+(\\w+|more\\s+in\\s+\\w+|like\\s+\\w+)\\b"

  # PATTERN 3: "[Country] - Full of [Human Quality]"
  pattern3 <- "\\b(full\\s+of|filled\\s+with)\\s+(life|energy|spirit|soul|warmth|love|joy|happiness|charm|character|personality|magic|wonder)\\b"

  # PATTERN 4: "Answer/Hear/Follow the call of [Place]"
  pattern4 <- "\\b(answer|hear|follow|heed)\\s+the\\s+call\\s+of\\b"

  # PATTERN 5: Possessive personification with expanded patterns
  pattern5 <- paste0(
    "\\b(", paste(non_human_subjects, collapse = "|"), ")'s\\s+",
    "(embrace|gift|whisper|call|invitation|welcome|smile|kiss|breath|heart|soul|spirit|arms|hands|voice|eyes|face|touch|warmth|love|charm|magic|secrets?|treasures?|beauty)"
  )

  # PATTERN 6: "[Place] has [Human Quality/Possession]"
  pattern6 <- "\\b(\\w+)\\s+has\\s+(an?\\s+)?(address|heart|soul|spirit|personality|character|charm|magic|secrets?|voice|face|arms|hands|eyes|smile|warmth|energy|life|breath|pulse)"

  # PATTERN 7: Emotional states and human qualities
  pattern7 <- "\\b(smiling|laughing|crying|sleeping|dreaming|breathing|living|loving|caring|charming|welcoming|inviting)\\s+(coast|shore|city|island|country|nation|place|destination|land)"

  # PATTERN 8: "Let [Feminine Pronoun] [Action]" (countries as feminine)
  pattern8 <- "\\blet\\s+(her|she)\\s+(\\w+)\\s+(you|us)\\b"

  # PATTERN 9: "[Place] [Human Emotion Verb] you" - Enhanced to require direct object
  pattern9 <- "\\b(\\w+)\\s+(loves?|misses?|wants?|needs?|awaits?|expects?|hopes?|wishes?|dreams?|longs?\\s+for)\\s+you\\b"

  # PATTERN 10: Abstract concepts with human actions
  pattern10 <- "\\b(beauty|warmth|culture|tradition|heritage|adventure|mystery|magic|wonder|charm|spirit|soul)\\s+(awaits?|calls?|whispers?|speaks?|invites?|welcomes?|embraces?|reaches?|touches?)\\b"

  # PATTERN 11: Human experience verbs applied to places (removed "experience" verb)
  pattern11 <- "\\b(you\\s+)?(have\\s+to\\s+|will\\s+|can\\s+|must\\s+)?(feel|live|breathe|taste|touch|sense)\\s+(\\w+|more\\s+in\\s+\\w+|the\\s+\\w+)\\b"

  # NEW PATTERN 12: Inclusive "we" constructions (place identifying with people)
  pattern12 <- "\\b(we\\s+(are|have|offer|provide|give|share|welcome|invite|embrace|love|celebrate|believe|know|understand)|our\\s+(people|culture|tradition|heritage|warmth|hospitality|spirit|soul|heart)|join\\s+us|come\\s+to\\s+us|be\\s+with\\s+us|one\\s+of\\s+us)\\b"

  # Common personification phrases in tourism (updated and expanded)
  common_personification <- c(
    "\\b\\w+\\s+awaits?\\b", # Broader: catches both "X awaits you" and "X awaits"
    "\\bcalls? to you\\b",
    "\\bwelcomes? you\\b",
    "\\binvites? you\\b",
    "\\bembraces? you\\b",
    "\\bkisses? the\\b",
    "\\bwhispers? to\\b",
    "\\bdances? with\\b",
    "\\bsings? to\\b",
    "\\bsmiles? at\\b",
    "\\bbreathes? life\\b",
    "\\bawakens? your\\b",
    "\\bstirs? your\\b",
    "\\btakes? your breath\\b",
    "\\bsteals? your heart\\b",
    "\\bcaptures? your\\b",
    "\\bspeaks? to your\\b",
    "\\btouches? your\\b",
    "\\bmoves? you\\b",
    "\\binspires? you\\b",
    "\\bsurprises? you\\b",
    "\\brewards? you\\b",
    "\\bblesses? you\\b",
    "\\bhas\\s+an?\\s+address\\b",
    "\\bhas\\s+a\\s+(heart|soul|spirit|personality|character)\\b",
    "\\bfull\\s+of\\s+(life|energy|spirit|soul|character|personality)\\b",
    "\\bsmiling\\s+(coast|shore|face|land)\\b",
    "\\blive\\s+love\\s+\\w+\\b", # "Live Love Lebanon"
    "\\blive\\s+it\\s+to\\s+believe\\b", # "Live it to believe it"
    "\\bnowhere\\s+like\\s+us\\b",
    "\\blive\\s+for\\s+more\\b",
    "\\byou\\s+have\\s+to\\s+feel\\s+it\\b",
    "\\bfeel\\s+more\\s+in\\b",
    "\\blet\\s+her\\s+inspire\\b",
    "\\bi\\s+feel\\s+like\\s+\\w+\\b",
    "\\banswer\\s+the\\s+call\\s+of\\b",
    "\\btake\\s+another\\s+look\\b", # Implies the place is showing itself
    "\\bvisit\\s+and\\s+love\\s+us\\b", # Place referring to itself as "us"
    "\\bcome\\s+and\\s+find\\s+your\\b", # More specific than just "find"
    "\\bwe\\s+are\\s+waiting\\b",
    "\\bour\\s+doors\\s+are\\s+open\\b",
    "\\bour\\s+arms\\s+are\\s+open\\b",
    # Direct address patterns with "our"
    "\\bour\\s+\\w+\\s+your\\s+\\w+\\b", # "Our Pride Your Destination"
    "\\bbe\\s+our\\s+guest\\b", # "Be Our Guest"
    "\\blove\\s+our\\s+\\w+\\b", # "Love our Little Paradise"
    "\\bvisit\\s+our\\s+\\w+\\b", # "Visit our country"
    "\\bdiscover\\s+our\\s+\\w+\\b", # "Discover our secrets"
    "\\bexplore\\s+our\\s+\\w+\\b", # "Explore our culture"
    "\\benjoy\\s+our\\s+\\w+\\b", # "Enjoy our hospitality"
    "\\bour\\s+\\w+\\s+(awaits?|welcomes?|invites?|calls?)\\b" # "Our paradise awaits"
  )

  # Test all patterns
  has_pattern1 <- grepl(pattern1, text_lower, perl = TRUE)
  has_pattern2 <- grepl(pattern2, text_lower, perl = TRUE)
  has_pattern3 <- grepl(pattern3, text_lower, perl = TRUE)
  has_pattern4 <- grepl(pattern4, text_lower, perl = TRUE)
  has_pattern5 <- grepl(pattern5, text_lower, perl = TRUE)
  has_pattern6 <- grepl(pattern6, text_lower, perl = TRUE)
  has_pattern7 <- grepl(pattern7, text_lower, perl = TRUE)
  has_pattern8 <- grepl(pattern8, text_lower, perl = TRUE)
  has_pattern9 <- grepl(pattern9, text_lower, perl = TRUE)
  has_pattern10 <- grepl(pattern10, text_lower, perl = TRUE)
  has_pattern11 <- grepl(pattern11, text_lower, perl = TRUE)
  has_pattern12 <- grepl(pattern12, text_lower, perl = TRUE)
  has_common <- any(sapply(common_personification, function(p) grepl(p, text_lower, perl = TRUE)))

  return(has_pattern1 || has_pattern2 || has_pattern3 || has_pattern4 ||
    has_pattern5 || has_pattern6 || has_pattern7 || has_pattern8 ||
    has_pattern9 || has_pattern10 || has_pattern11 || has_pattern12 ||
    has_common)
}

detect_local_language <- function(text) {
  if (is.na(text) || text == "") {
    return(FALSE)
  }

  # Convert to lowercase for pattern matching
  text_lower <- tolower(text)

  # Define place names and English words to exclude
  place_names_to_exclude <- c(
    # Country/territory names
    "\\bsamoa\\b", "\\btahiti\\b", "\\bfiji\\b", "\\bhawaii\\b", "\\bcuba\\b",
    "\\bmartinique\\b", "\\bguadeloupe\\b", "\\breunion\\b", "\\bluxembourg\\b",
    "\\bbelgique\\b", "\\bsuisse\\b", "\\bschweiz\\b", "\\bosterreich\\b",
    "\\bel salvador\\b", "\\bcote d'ivoire\\b", "\\bcabo verde\\b",
    # English words that might trigger false positives
    "\\bharrive\\b", "\\birresistible\\b", "\\bnaturally\\b"
  )

  # First check if text contains only place names we want to exclude
  place_name_pattern <- paste(place_names_to_exclude, collapse = "|")
  if (grepl(place_name_pattern, text_lower)) {
    # If it contains excluded place names, remove them and check if anything foreign remains
    text_without_places <- gsub(place_name_pattern, "", text_lower)
    text_without_places <- trimws(gsub("[[:space:]]+", " ", text_without_places))

    # If after removing place names there's nothing substantial left, return FALSE
    if (nchar(text_without_places) < 3) {
      return(FALSE)
    }

    # Continue analysis with the text that has place names removed
    text_lower <- text_without_places
  }

  # 1. Non-ASCII characters (accented characters, etc.)
  has_non_ascii <- grepl("[^\x01-\x7F]", text)

  # 2. Common non-English words and phrases (excluding place names already handled above)
  foreign_patterns <- c(
    # Spanish - but exclude standalone "el" when it's part of country names
    "\\bpura vida\\b", "\\bviva\\b", "\\bvive\\b", "\\bmucho\\b", "\\bmas\\b",
    "\\bla\\b", "\\bde\\b", "\\by\\b", "\\ben\\b", "\\bcon\\b",
    "\\bauthenti[ck]a\\b", "\\brojo\\b", "\\bazul\\b",
    "\\btierra\\b", "\\bmar\\b", "\\bsol\\b", "\\bluna\\b", "\\bcielo\\b",

    # French - but more restrictive
    "\\bla\\b", "\\ble\\b", "\\bet\\b", "\\bdu\\b", "\\bdes\\b",
    "\\bun\\b", "\\bune\\b", "\\baux\\b", "\\bvivre\\b", "\\bl'instant\\b",
    "\\bl'île\\b", "\\bîle\\b", "\\bfleurs\\b", "\\bintense\\b",
    "\\bterre\\b", "\\bmer\\b",

    # Pacific Languages (Fijian, Samoan, etc.) - but only actual language words, not place names
    "\\bmotu\\b", "\\bfiafiaga\\b", "\\bbula\\b", "\\bvinaka\\b",
    "\\btalofa\\b", "\\baloha\\b", "\\bmahalo\\b",

    # Portuguese - more restrictive
    "\\bbrasileiro\\b", "\\bpraia\\b", "\\bvida\\b",

    # Italian - more restrictive
    "\\bbella\\b", "\\bbello\\b", "\\bamore\\b", "\\bmare\\b",

    # German - more restrictive
    "\\bdas\\b", "\\bder\\b", "\\bdie\\b", "\\bund\\b", "\\bschön\\b",

    # Dutch - more restrictive
    "\\bhet\\b", "\\bvan\\b", "\\bmooi\\b", "\\bwater\\b",

    # Nordic languages
    "\\bdet\\b", "\\boch\\b", "\\batt\\b", "\\bför\\b", "\\btill\\b",

    # Other common patterns
    "\\bbon\\b", "\\bbien\\b", "\\bbueno\\b", "\\bbom\\b", "\\bgut\\b"
  )

  # 3. Common greeting/hospitality terms in local languages
  hospitality_terms <- c(
    "\\bbienvenu\\b", "\\bbienvenid\\b", "\\bwelkom\\b", "\\bwillkommen\\b",
    "\\bbenvenut\\b", "\\bkaribuni\\b"
  )

  # Combine language patterns (excluding place names and hospitality terms that are just greetings)
  language_patterns <- c(foreign_patterns, hospitality_terms)

  # Check for any pattern match
  has_foreign_words <- any(sapply(language_patterns, function(pattern) grepl(pattern, text_lower)))

  # 4. Letter combinations uncommon in English - but be more restrictive
  uncommon_combinations <- c(
    "ç", "ñ", "ü", "ö", "ä", "å", "ø", "æ", # Special characters
    "\\bll\\b", # Spanish ll as standalone word
    "ão\\b", # Portuguese ending
    "ção\\b" # Portuguese ending
  )

  # Be more restrictive with "rr" - only flag if it's not in common English words
  english_rr_words <- c("arrive", "arrival", "correct", "error", "sorry", "carry", "marry", "berry", "merry", "ferry", "irresistible", "irritate", "arrange", "barrier", "current", "hurricane")
  has_rr <- grepl("rr", text_lower)
  if (has_rr) {
    # Check if the "rr" is in a common English word
    is_english_rr <- any(sapply(english_rr_words, function(word) grepl(paste0("\\b", word, "\\b"), text_lower)))
    if (is_english_rr) {
      has_rr <- FALSE # Don't count it as foreign if it's in a common English word
    }
  }

  has_uncommon_combos <- has_rr || any(sapply(uncommon_combinations, function(pattern) grepl(pattern, text_lower)))

  return(has_non_ascii || has_foreign_words || has_uncommon_combos)
}

# Find Respresntative Slogans
calculate_representative_slogans <- function(data) {
  req(data)
  if (!all(c("valence", "arousal", "dominance", "cluster", "slogan") %in% colnames(data))) {
    return(NULL)
  }
  tryCatch(
    {
      clean_data <- data %>%
        filter(!is.na(valence) & !is.na(arousal) & !is.na(dominance))
      if (nrow(clean_data) == 0) {
        return(NULL)
      }
      clean_data %>%
        group_by(cluster) %>%
        group_modify(~ {
          cluster_data <- .x
          dist_matrix <- as.matrix(daisy(cluster_data %>% select(valence, arousal, dominance)))
          # Sum of dissimilarities for each slogan
          row_sums <- rowSums(dist_matrix, na.rm = TRUE)
          # Order indices by closeness (lowest sum first)
          order_idx <- order(row_sums)
          tibble(
            representative_slogan = cluster_data$slogan[order_idx[1]],
            second_closest_slogan = if (length(order_idx) > 1) cluster_data$slogan[order_idx[2]] else NA_character_,
            third_closest_slogan = if (length(order_idx) > 2) cluster_data$slogan[order_idx[3]] else NA_character_
          )
        }) %>%
        ungroup()
    },
    error = function(e) {
      message("Error calculating representative slogans: ", e$message)
      return(NULL)
    }
  )
}

fix_iso_codes <- function(data) {
  # Fix Namibia's NAM issue (was conflicting with NA)
  namibia_rows <- which(data$country == "Namibia" |
    grepl("Namibia", data$country, ignore.case = TRUE))
  if (length(namibia_rows) > 0) {
    data$iso_alpha3[namibia_rows] <- "NAM"
    cat("✓ Fixed Namibia ISO code:", length(namibia_rows), "rows\n")
  }

  # Manual fixes for other unmatched countries using ISO Alpha-3
  iso_fixes <- list(
    "French Guiana" = "GUF",
    "Guadeloupe" = "GLP",
    "Martinique" = "MTQ",
    "Réunion" = "REU",
    "Taiwan" = "TWN",
    "Puerto Rico" = "PRI",
    "United States Virgin Islands" = "VIR",
    "British Virgin Islands" = "VGB",
    "Cayman Islands" = "CYM",
    "Turks and Caicos Islands" = "TCA",
    "Bermuda" = "BMU",
    "Greenland" = "GRL",
    "Faroe Islands" = "FRO",
    "Gibraltar" = "GIB",
    "Isle of Man" = "IMN",
    "Jersey" = "JEY",
    "Guernsey" = "GGY",
    "Åland Islands" = "ALA",
    "American Samoa" = "ASM",
    "Guam" = "GUM",
    "Northern Mariana Islands" = "MNP",
    "Cook Islands" = "COK",
    "Niue" = "NIU",
    "Tokelau" = "TKL",
    "Wallis and Futuna" = "WLF",
    "Saint Pierre and Miquelon" = "SPM",
    "Saint Barthélemy" = "BLM",
    "Saint Martin" = "MAF",
    "Aruba" = "ABW",
    "Curaçao" = "CUW",
    "Sint Maarten" = "SXM",
    "Caribbean Netherlands" = "BES"
  )

  for (country_name in names(iso_fixes)) {
    matching_rows <- which(grepl(country_name, data$country, ignore.case = TRUE))
    if (length(matching_rows) > 0) {
      data$iso_alpha3[matching_rows] <- iso_fixes[[country_name]]
      cat("✓ Fixed", country_name, "ISO code:", length(matching_rows), "rows\n")
    }
  }

  return(data)
}

# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Tourism Slogan Cluster Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "data_input", icon = icon("upload")),
      menuItem("Cluster Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Word Frequency", tabName = "frequency", icon = icon("list-ol")),
      menuItem("Word Clouds", tabName = "wordclouds", icon = icon("cloud")),
      menuItem("Thematic Patterns", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Linguistic Features", tabName = "linguistic", icon = icon("language")),
      menuItem("Thematic & Linguistic Overview", tabName = "overview_patterns", icon = icon("search")),
      menuItem("Geographic Information", tabName = "geographic", icon = icon("globe")),
      menuItem("Export Results", tabName = "export", icon = icon("download"))
    )
  ),
  dashboardBody(
    tags$style(HTML("
    .map-container {
        width: 100%;
        height: 100%;
    }
    .legend-container {
        display: flex;
        justify-content: center;
        margin-bottom: 15px;
    }
    .legend-item {
        display: flex;
        align-items: center;
        margin: 0 15px;
    }
    .legend-color {
        width: 20px;
        height: 20px;
        margin-right: 8px;
        border: 1px solid #333;
    }
    .legend-text {
        font-size: 14px;
        font-weight: bold;
    }
  ")),
    tabItems(
      # Data Input Tab
      tabItem(
        tabName = "data_input",
        fluidRow(
          box(
            title = "Upload Data",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fileInput(
              "file",
              "Choose CSV or Excel File",
              accept = c(".csv", ".xls", ".xlsx")
            ),
            helpText(
              "CSV should contain columns: country, slogan, cluster, wc, cw, cwr, valence, arousal, dominance, ranking, region, sub_region"
            ),
            br(),
            conditionalPanel(
              condition = "output.fileUploaded",
              h4("Data Preview:"),
              DT::dataTableOutput("data_preview")
            )
          )
        ),
        fluidRow(
          box(
            title = "Sample Data Format",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            HTML(
              "
            <table class='table table-striped'>
            <thead><tr><th>country</th><th>slogan</th><th>cluster</th><th>wc</th><th>cw</th><th>cwr</th><th>valence</th><th>arousal</th><th>dominance</th><th>ranking</th><th>region</th><th>sub_region</th><th>iso_alpha3</th></tr></thead>
            <tbody>
            <tr><td>Thailand</td><td>Amazing Thailand</td><td>1</td><td>2</td><td>2</td><td>0.85</td><td>12.13</td><td>10.28</td><td>5.43</td><td>2</td><td>Asia</td><td>South-Eastern Asia</td><td>THA</td></tr>
            <tr><td>Malaysia</td><td>Malaysia Truly Asia</td><td>3</td><td>3</td><td>3</td><td>0.75</td><td>5.13</td><td>11.24</td><td>9.78</td><td>75</td><td>Asia</td><td>South-Eastern Asia</td><td>MYS</td></tr>
            <tr><td>Japan</td><td>Visit Japan</td><td>3</td><td>2</td><td>2</td><td>0.65</td><td>10.78</td><td>6.43</td><td>7.09</td><td>135</td><td>Asia</td><td>Eastern Asia</td><td>JPN</td></tr>
            <tr><td>France</td><td>Rendez-vous en France</td><td>2</td><td>3</td><td>2</td><td>0.70</td><td>5.50</td><td>5.05</td><td>12.33</td><td>48</td><td>Europe</td><td>Western Europe</td><td>FRA</td></tr>
            <tr><td>French Guiana</td><td>Guyane Française Naturellement</td><td>1</td><td>3</td><td>3</td><td>1.00</td><td>6.50</td><td>4.25</td><td>8.75</td><td>180</td><td>South America</td><td>South America</td><td>GUF</td></tr>
            </tbody>
            </table>
            "
            )
          )
        )
      ),

      # Cluster Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_countries"),
          valueBoxOutput("total_clusters"),
          valueBoxOutput("avg_words")
        ),
        fluidRow(
          box(
            title = "Cluster Statistics",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            withSpinner(DT::dataTableOutput("cluster_stats"))
          )
        ),
        fluidRow(
          box(
            title = "3D Cluster Plot",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("cluster_3d_plot"))
          ),
          box(
            title = "2D Cluster Plot",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("valence_arousal_plot"))
          )
        ),
        fluidRow(
          box(
            title = "3D Plot Controls",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE, # Changed from TRUE to FALSE to show by default
            radioButtons("center_type", "Display Centers As:",
              choices = list(
                "Centroids (Mean)" = "centroids",
                "Medoids (Actual Points)" = "medoids",
                "Both" = "both",
                "None" = "none"
              ),
              selected = "medoids",
              inline = TRUE
            ) # Added inline = TRUE for horizontal layout
          )
        )
      ),

      # Word Frequency Tab
      tabItem(
        tabName = "frequency",
        fluidRow(
          box(
            title = "Controls",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            selectInput("selected_cluster", "Select Cluster:", choices = NULL),
            numericInput(
              "top_n_words",
              "Number of Top Words:",
              value = 20,
              min = 5,
              max = 50
            ),
            checkboxInput("remove_stopwords", "Remove Common Words", value = FALSE)
          ),
          box(
            title = "Word Frequency",
            status = "success",
            solidHeader = TRUE,
            width = 9,
            withSpinner(plotlyOutput("frequency_plot"))
          )
        ),
        fluidRow(
          box(
            title = "Top Words Table",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            withSpinner(DT::dataTableOutput("frequency_table"))
          )
        )
      ),

      # Word Clouds Tab - FIXED LAYOUT
      tabItem(
        tabName = "wordclouds",
        fluidRow(
          box(
            title = "Word Cloud Controls",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            column(
              4,
              numericInput(
                "max_words",
                "Maximum Words:",
                value = 20,
                min = 10,
                max = 100
              )
            ),
            column(
              4,
              selectInput(
                "color_palette",
                "Color Palette:",
                choices = list(
                  "Dark2" = "Dark2",
                  "Set1" = "Set1",
                  "Paired" = "Paired"
                )
              )
            ),
            column(
              4,
              checkboxInput("remove_common", "Remove Common Words", value = TRUE)
            )
          )
        ),
        fluidRow(
          box(
            title = "Comparative Word Clouds",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(
                4,
                h4("Cluster 1", style = "color: #ff7f0e; text-align: center;"),
                withSpinner(plotOutput("wordcloud1", height = "400px"))
              ),
              column(
                4,
                h4("Cluster 2", style = "color: #2ca02c; text-align: center;"),
                withSpinner(plotOutput("wordcloud2", height = "400px"))
              ),
              column(
                4,
                h4("Cluster 3", style = "color: #0000ff; text-align: center;"),
                withSpinner(plotOutput("wordcloud3", height = "400px"))
              )
            )
          )
        )
      ),

      # Thematic Patterns Tab
      tabItem(
        tabName = "comparison",
        fluidRow(
          box(
            title = "Distinctive Words by Cluster",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            withSpinner(plotlyOutput("keyness_plot", height = "500px"))
          )
        ),
        fluidRow(
          box(
            title = "Thematic Analysis",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            withSpinner(plotlyOutput("thematic_plot"))
          )
        )
      ),

      # Linguistic Features Tab
      tabItem(
        tabName = "linguistic",
        fluidRow(
          box(
            title = "Linguistic Pattern Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            withSpinner(DT::dataTableOutput("linguistic_features"))
          )
        ),
        fluidRow(
          box(
            title = "Structural Features Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("structural_plot"))
          ),
          box(
            title = "Semantic Features Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("semantic_plot"))
          )
        ),
        fluidRow(
          box(
            title = "Linguistic & Cultural Markers Distribution",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            withSpinner(plotlyOutput("markers_plot"))
          )
        )
      ),

      # Thematic & Linguistic Overview Tab
      tabItem(
        tabName = "overview_patterns",
        fluidRow(
          box(
            title = "Thematic Overview",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "450px",
            fluidRow(
              column(
                4,
                selectInput("selected_theme", "Select Thematic Pattern:",
                  choices = c(
                    "Nature" = "nature",
                    "Culture" = "culture",
                    "Urban" = "urban",
                    "Adventure" = "adventure",
                    "Relaxation" = "relaxation",
                    "Luxury" = "luxury",
                    "Friendliness" = "friendliness",
                    "Romantic" = "romantic",
                    "Geographical" = "geographical",
                    "Positive Emotion" = "positive_emotion",
                    "Excitement" = "excitement",
                    "Authenticity" = "authenticity",
                    "Action Verbs" = "action_verbs",
                    "Uniqueness" = "uniqueness",
                    "Superlatives" = "superlatives"
                  )
                )
              )
            ),
            withSpinner(DT::dataTableOutput("thematic_pattern_table"))
          )
        ),
        fluidRow(
          box(
            title = "Linguistic Overview",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            height = "450px",
            fluidRow(
              column(
                4,
                selectInput("selected_linguistic", "Select Linguistic Feature:",
                  choices = c(
                    "Imperative" = "has_imperative",
                    "Interrogative" = "has_interrogative",
                    "Rhyme" = "has_rhyme",
                    "Alliteration" = "has_alliteration",
                    "Personification" = "has_personification",
                    "Sensory Words" = "has_sensory",
                    "Metaphorical" = "has_metaphor",
                    "Simile" = "has_simile",
                    "Hyperbolic" = "has_hyperbole",
                    "Local Language" = "has_local_lang",
                    "Indigenous Terms" = "has_indigenous",
                    "Inclusive We" = "has_inclusive",
                    "Direct You" = "has_direct",
                    "Submissive Her" = "has_submissive"
                  )
                )
              )
            ),
            withSpinner(DT::dataTableOutput("linguistic_pattern_table"))
          )
        )
      ),

      # Geographic Information Tab
      tabItem(
        tabName = "geographic",
        fluidRow(
          box(
            title = "Geographic Filters",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(4, selectInput(
                "geo_cluster",
                "Select Cluster:",
                choices = c(
                  "All" = "all",
                  "1" = "1",
                  "2" = "2",
                  "3" = "3"
                )
              )),
              column(4, selectInput("geo_region", "Select Region:", choices = NULL)),
              column(
                4,
                selectInput("geo_subregion", "Select Sub-Region:", choices = NULL)
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Geographic Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 12, # Keep full width
            height = "750px", # Slightly increased height to accommodate note
            div(
              class = "map-container",
              withSpinner(plotlyOutput("world_map", height = "650px"))
            ),
            # Add the note about French overseas territories
            div(
              style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px;",
              tags$p(
                tags$strong("Note:"),
                "French overseas territories (French Guiana, Guadeloupe, Martinique, and Réunion) are grouped under France in the map visualization. Users are advised to check the 'Selected Countries' table below for detailed information on individual territories.",
                style = "margin: 0; font-size: 14px; color: #495057;"
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Cluster Summary Statistics",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            withSpinner(DT::dataTableOutput("cluster_summary_table"))
          )
        ),
        fluidRow(
          box(
            title = "ISO Matching Diagnostics",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE, # Collapsed by default so it doesn't take up space
            withSpinner(DT::dataTableOutput("iso_matching_diagnostics"))
          )
        ),
        fluidRow(
          box(
            title = "Selected Countries",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            withSpinner(DT::dataTableOutput("selected_countries"))
          )
        ),
      ),

      # Export Results Tab
      tabItem(
        tabName = "export",
        fluidRow(
          box(
            title = "Export Results",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            h4("Download Analysis Results:"),
            br(),
            downloadButton("download_summary", "Download Summary Statistics", class = "btn-primary"),
            br(), br(),
            downloadButton("download_frequency", "Download Word Frequencies", class = "btn-success"),
            br(), br(),
            downloadButton("download_keyness", "Download Distinctive Words", class = "btn-warning"),
            br(), br(),
            downloadButton("download_thematic", "Download Thematic Distribution", class = "btn-info"),
            br(), br(),
            downloadButton("download_linguistic", "Download Linguistic Features Summary", class = "btn-info"),
            br(), br(),
            downloadButton("download_linguistic_breakdown", "Download Linguistic Feature Breakdown", class = "btn-danger"),
            br(), br(),
            downloadButton("download_dissimilarity", "Download Dissimilarity Distances", class = "btn-secondary"),
            h4("Analysis Summary:"),
            verbatimTextOutput("analysis_summary")
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  # Define consistent color scheme for clusters
  cluster_colors <- c(
    "1" = "#ff7f0e",
    # Orange
    "2" = "#2ca02c",
    # Green
    "3" = "#0000ff"
  ) # Blue

  # Comprehensive tourism discourse dictionary (expanded)
  tourism_dict <- dictionary(
    list(
      # Destination Attributes
      nature = c(
        "\\bnature\\w*\\b",
        "\\bnatural\\w*\\b",
        "\\bwild\\w*\\b",
        "\\beco\\w*\\b", # This will now only match complete words starting with "eco"
        "\\bgreen\\w*\\b",
        "\\bsustainab\\w*\\b",
        "\\bmountain\\w*\\b",
        "\\bforest\\w*\\b",
        "\\bjungle\\w*\\b",
        "\\bpark\\w*\\b",
        "\\breserve\\w*\\b",
        "\\bbeach\\w*\\b",
        "\\bcoast\\w*\\b",
        "\\bshore\\w*\\b",
        "\\bisland\\w*\\b",
        "\\bocean\\w*\\b",
        "\\bsea\\w*\\b",
        "\\blake\\w*\\b",
        "\\briver\\b", "\\brivers\\b",
        "\\bwaterfall\\w*\\b",
        "\\bdesert\\w*\\b",
        "\\bcave\\w*\\b",
        "\\bvolcan\\w*\\b"
      ),
      culture = c(
        "culture*",
        "heritage*",
        "tradition*",
        "history*",
        "historic*",
        "ancient*",
        "\\bart\\b",
        "\\bland\\b",
        "museum*",
        "gallery*",
        "festival*",
        "custom*",
        "indigen*",
        "ethnic*",
        "archeolog*",
        "monument*",
        "temple*",
        "church*",
        "mosque*",
        "synagog*",
        "shrine*"
      ),
      urban = c(
        "\\bcity\\b",
        "\\bmetropol\\w*\\b",
        "\\burban\\b",
        "\\bskyline\\b",
        "\\barchitecture\\b",
        "\\bmodern\\b",
        "\\bcontempor\\w*\\b",
        "\\bdesign\\b",
        "\\bfashion\\b",
        "\\bshopping\\b",
        "\\bmall\\b",
        "\\bnightlife\\b",
        "\\bclub\\b",
        "\\bbar\\b",
        "\\brestaurant\\b"
      ),

      # Experience Types
      adventure = c(
        "\\badventure\\w*\\b",
        "explor*",
        "discover*",
        "journey*",
        "expedition*",
        "trek*",
        "hike*",
        "climb*",
        "raft*",
        "kayak*",
        "\\dive\\w*\\b",
        "safari*",
        "wildlife*",
        "outdoor*",
        "extrem*",
        "challenge*"
      ),
      relaxation = c(
        "\\brelax\\b", "\\brelaxing\\b",
        "serene*",
        "peace*",
        "tranquil*",
        "\\bcalm\\b",
        "\\bspa\\b",
        "\\bretreat\\w*\\b",
        "wellness*",
        "rejuvenat*",
        "\\bzen\\b",
        "meditat*",
        "\\byoga\\b",
        "refresh*",
        "unwind*"
      ),
      luxury = c(
        "\\bluxury\\b",
        "\\bexclusive\\b",
        "\\bpremium\\b",
        "\\belite\\b",
        "\\bvip\\b",
        "\\bboutique\\b",
        "\\bchic\\b",
        "\\belegant\\b",
        "\\bsophisticated\\b",
        "\\bglamor\\b", "\\bglamorous\\b",
        "\\bopulenT\\b",
        "\\bpamper\\b",
        "\\bindulge\\b"
      ),
      friendliness = c(
        "\\bfriendly\\b",
        "\\bfriendliness\\b",
        "\\bwelcome\\b",
        "warm*",
        "\\bhospitable\\b",
        "\\bhospitality\\b",
        "\\bkind\\b",
        "\\caring\\b",
        "helpful*",
        "inviting*",
        "embracing*",
        "open*",
        "smiling*",
        "cheerful*",
        "pleasant*",
        "cordial*",
        "gracious*",
        "accommodating*"
      ),
      romantic = c(
        "romance*",
        "honeymoon*",
        "couple*",
        "\\blove\\b", "\\bloved\\b", "\\bloving\\b", "\\bloves\\b",
        "\\bsLOVEnia\\b", # For cases like "sLOVEnia"
        "intimate*",
        "getaway*",
        "escape*",
        "valentine*",
        "\\byour heart\\b",
        "\\bwarm heart\\b",
        "\\bfrom the heart\\b",
        "passion*",
        "romantic*"
      ),

      # Emotional Appeals
      positive_emotion = c(
        "amaz*",
        "wonder*",
        "fantast*",
        "incredib*",
        "spectacular*",
        "\\bbeautif\\w*\\b", # Fixed: beautiful, beautify (not "better")
        "\\bbeauty\\b", # Fixed: separate pattern for "beauty"
        "\\bbreathtaking\\b", # Fixed: full word only
        "stunn*",
        "majest*",
        "paradise*",
        "dream*",
        "magic*",
        "enchant*",
        "heaven*",
        "bliss*",
        "\\beden\\b"
      ),
      excitement = c(
        "excit*",
        "thrill*",
        "energy*",
        "vibrant*",
        "dynamic*",
        "passion*",
        "intens*",
        "electr*",
        "adrenal*"
      ),
      authenticity = c(
        "authentic*",
        "genuine*",
        "\\breal\\b",
        "\\boriginal\\b",
        "unspoiled*",
        "pristine*",
        "\\buntouched\\b",
        "tradition*",
        "heritage*"
      ),
      action_verbs = c(
        "\\bvisit\\b", "\\bvisiting\\b", "\\bvisited\\b",
        "\\bcome\\b", "\\bcoming\\b",
        "\\bgo\\b", "\\bgoing\\b", "\\bgone\\b",
        "\\bexplor\\w*\\b",
        "\\bdiscover\\w*\\b",
        "\\bexperien\\w*\\b",
        "\\bsee\\b", "\\bseeing\\b", "\\bseen\\b",
        "\\bfeel\\b", "\\bfeeling\\b", "\\bfelt\\b",
        "\\bfind\\b", "\\bfinding\\b", "\\bfound\\b",
        "\\btaste\\b", "\\btasting\\b", "\\btasted\\b",
        "\\bsmell\\b", "\\bsmelling\\b",
        "\\bhear\\b", "\\bhearing\\b", "\\bheard\\b",
        "\\btouch\\b", "\\btouching\\b", "\\btouched\\b",
        "\\bembrace\\b", "\\bembracing\\b", "\\bembraced\\b",
        "\\bimmers\\w*\\b",
        "\\bwitness\\b", "\\bwitnessing\\b", "\\bwitnessed\\b",
        "\\bencounter\\b", "\\bencountering\\b", "\\bencountered\\b"
      ),

      # Temporal References
      timeless = c("forever*", "eternal*", "always*", "never-end*", "permanen*"),
      seasonal = c(
        "summer*",
        "winter*",
        "spring*",
        "autumn*",
        "fall*",
        "season*",
        "year-round",
        "sunny",
        "snow*"
      ),

      # Geographical References
      geographical = c(
        "\\bcenter\\b",
        "crossroad*",
        "\\bgateway to\\b"
      ),

      # Uniqueness Claims
      uniqueness = c(
        "unique*",
        "only",
        "exclusiv*",
        "special*",
        "distinct*",
        "unlike*",
        "different*",
        "rare*",
        "one-of-a-kind",
        "singular*"
      ),

      # Superlatives
      superlatives = c(
        "best",
        "greatest",
        "most",
        "finest",
        "\\btop\\b",
        "ultimate",
        "perfect",
        "ideal",
        "premier",
        "leading",
        "first",
        "number one",
        "100%",
        "#1"
      )
    )
  )

  # Enhanced regex patterns for linguistic features (UPDATED VERSION)
  linguistic_patterns <- list(
    # Structural patterns
    imperative = "^(Visit|Come|Go|Discover|Explore|Experience|See|Feel|Find|Taste|Enjoy|Welcome|Let's)",
    interrogative = "\\?$",
    # Note: rhyming, alliteration, and simile now handled by custom functions above

    # Semantic patterns
    sensory_words = "\\b(taste|smell|hear|see|touch|feel|savor|listen|view|watch|observe|witness)\\b",
    metaphorical = "\\b(paradise|gem|jewel|pearl|haven|eden|oasis|treasure|dream|magic|enchant|heart|sleepy
    |ooze|foot|breath|dozy|finger|body|exude|freckled|frisson|heart-warming|mouth|nudged|pulse|teardrop|toe
    |emerald|turquoise|gold|crystal|platinum|sapphire|gemstone|silver|ivory|amber|bronze|alabaster|silica|jade
    |topaz|pearly|golden|green|color|white|honey-colored|blue|mellow|pastels|faded|burnt-orange|scarlet|flavor
    |taste|delicious|deliciously|sweet|sugary|spicy|aromas|sweetness|heaven|blessed|soul|worshipping|sun-worship
    |spirit)\\b",
    hyperbolic = "\\b(amazing|incredible|unforgettable|breathtaking|spectacular|mind-blowing|extraordinary)\\b",

    # Cultural references

    indigenous_terms = "\\b(ayurveda|safari|kimono|sushi|tapas|siesta|fjord|outback|masai|samurai)\\b",

    # Temporal patterns
    time_reference = "\\b(now|today|always|forever|eternal|never|season|summer|winter|spring|autumn)\\b",

    # Pronoun usage
    inclusive_we = "\\b(we|our|us|ourselves|together)\\b",
    direct_you = "\\b(you|your|yours|yourself)\\b",
    submissive_her = "\\b(she|her|hers|herself)\\b"
  )

  # Reactive values to store data
  values <- reactiveValues(
    data = NULL,
    corpus = NULL,
    processed = FALSE,
    world_map = NULL,
    representative_slogans = NULL
  )

  # Load world map data once
  observe({
    tryCatch(
      {
        # First, let's see what columns are available
        world_raw <- ne_countries(scale = "medium", returnclass = "sf")
        cat("Available columns in Natural Earth data:\n")
        cat(paste(colnames(world_raw), collapse = ", "), "\n")

        # Check if name_long exists, otherwise use name
        if ("name_long" %in% colnames(world_raw)) {
          map_unit_col <- "name_long"
          cat("Using name_long as map_unit\n")
        } else if ("NAME_LONG" %in% colnames(world_raw)) {
          map_unit_col <- "NAME_LONG"
          cat("Using NAME_LONG as map_unit\n")
        } else if ("NAME" %in% colnames(world_raw)) {
          map_unit_col <- "NAME"
          cat("Using NAME as map_unit\n")
        } else {
          map_unit_col <- "name" # fallback to basic name
          cat("Using name as map_unit (fallback)\n")
        }

        values$world_map <- world_raw %>%
          select(name, all_of(map_unit_col), iso_a3, geometry) %>%
          rename(map_unit = !!map_unit_col) %>% # Rename to consistent map_unit
          mutate(
            name = as.character(name),
            map_unit = as.character(map_unit),
            iso_a3 = as.character(iso_a3)
          ) %>%
          # Fix common world map issues for ISO Alpha-3
          mutate(
            iso_a3 = case_when(
              name == "France" & iso_a3 != "FRA" ~ "FRA",
              name == "Norway" & iso_a3 != "NOR" ~ "NOR",
              name == "Taiwan" ~ "TWN",
              grepl("French Guiana", map_unit, ignore.case = TRUE) ~ "GUF",
              grepl("Guadeloupe", map_unit, ignore.case = TRUE) ~ "GLP",
              grepl("Martinique", map_unit, ignore.case = TRUE) ~ "MTQ",
              grepl("Réunion|Reunion", map_unit, ignore.case = TRUE) ~ "REU",
              TRUE ~ iso_a3
            )
          )

        cat("World map loaded successfully with", nrow(values$world_map), "features\n")
      },
      error = function(e) {
        cat("Error loading world map:", e$message, "\n")
        # Create a minimal fallback
        values$world_map <- data.frame(
          name = character(0),
          map_unit = character(0),
          iso_a3 = character(0),
          geometry = character(0)
        )
      }
    )

    # Debug: Check ISO matching when both data and map are available
    if (!is.null(values$data)) {
      # Check how many ISO codes match
      data_iso <- unique(values$data$iso_alpha3)
      map_iso <- unique(values$world_map$iso_a3)

      matched_iso <- intersect(data_iso, map_iso)
      unmatched_iso <- setdiff(data_iso, map_iso)

      cat("=== ISO Alpha-3 Matching Report ===\n")
      cat("Total countries in your data:", length(data_iso), "\n")
      cat("Successfully matched by ISO:", length(matched_iso), "\n")
      cat("Unmatched ISO codes:", length(unmatched_iso), "\n")

      if (length(unmatched_iso) > 0) {
        cat("Unmatched ISO codes:", paste(unmatched_iso, collapse = ", "), "\n")

        # Show corresponding country names for unmatched ISO codes
        unmatched_countries <- values$data[values$data$iso_alpha3 %in% unmatched_iso, c("country", "iso_alpha3")]
        cat("Unmatched countries:\n")
        print(unique(unmatched_countries))
      }
      cat("======================================\n")
    }
  })

  # File upload handling
  observeEvent(input$file, {
    req(input$file)

    tryCatch(
      {
        ext <- tools::file_ext(input$file$name)

        if (ext == "csv") {
          values$data <- read.csv(
            input$file$datapath,
            stringsAsFactors = FALSE,
            fileEncoding = "UTF-8"
          )
        } else if (ext %in% c("xls", "xlsx")) {
          values$data <- read_excel(input$file$datapath)
        } else {
          showNotification("Please upload a CSV or Excel file",
            type = "error",
            duration = 5
          )
          return()
        }

        # Clean slogans - remove problematic characters
        values$data$slogan <- iconv(values$data$slogan, "UTF-8", "ASCII//TRANSLIT")
        values$data$slogan <- gsub("[^[:alnum:][:space:]'-]", "", values$data$slogan)

        # Updated required columns to include iso_alpha3
        required_cols <- c(
          "country", "slogan", "cluster", "wc", "cw", "cwr",
          "valence", "arousal", "dominance", "region", "sub_region", "iso_alpha3" # Changed from iso_alpha2
        )
        if (!all(required_cols %in% colnames(values$data))) {
          showNotification(
            paste(
              "Error: Missing required columns. Needed:",
              paste(required_cols, collapse = ", ")
            ),
            type = "error",
            duration = 5
          )
          return()
        }

        # Ensure numeric columns are properly formatted
        numeric_cols <- c("wc", "cw", "cwr", "valence", "arousal", "dominance", "ranking")
        for (col in numeric_cols) {
          if (col %in% colnames(values$data)) {
            values$data[[col]] <- as.numeric(values$data[[col]])
          }
        }

        # Clean ISO codes BEFORE applying fixes - now for Alpha-3
        values$data$iso_alpha3 <- toupper(trimws(as.character(values$data$iso_alpha3)))

        # Apply the ISO fixes
        values$data <- fix_iso_codes(values$data)

        # Create corpus
        values$corpus <- corpus(values$data,
          text_field = "slogan",
          docid_field = "country"
        )
        docvars(values$corpus, "cluster") <- values$data$cluster
        values$processed <- TRUE

        # Calculate and store representative slogans
        values$representative_slogans <- calculate_representative_slogans(values$data)

        # Update cluster choices
        updateSelectInput(session,
          "selected_cluster",
          choices = c("All" = "all", paste("Cluster", sort(
            unique(values$data$cluster)
          )))
        )

        # Update geographic filter choices
        updateSelectInput(session, "geo_region", choices = c("All" = "all", sort(unique(
          values$data$region
        ))))

        updateSelectInput(session, "geo_subregion", choices = c("All" = "all", sort(unique(
          values$data$sub_region
        ))))

        showNotification("Data loaded successfully!", type = "message")
      },
      error = function(e) {
        if (grepl("invalid multibyte string", e$message)) {
          showNotification(
            "Character encoding issue detected. Please ensure your file uses UTF-8 encoding.",
            type = "error",
            duration = 10
          )
        } else {
          showNotification(
            paste("Error loading file:", e$message),
            type = "error",
            duration = 5
          )
        }
      }
    )
  })

  # Check if file is uploaded
  output$fileUploaded <- reactive({
    return(!is.null(values$data))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

  # Data preview
  output$data_preview <- DT::renderDataTable({
    req(values$data)

    DT::datatable(
      head(values$data, 200),
      options = list(
        scrollX = TRUE,
        pageLength = 20,
        columnDefs = list(list(
          targets = c(0, 1),
          searchable = TRUE
        )),
        search = list(
          smart = TRUE,
          regex = TRUE,
          caseInsensitive = TRUE
        )
      ),
      colnames = c(
        "Country",
        "Slogan",
        "Cluster",
        "WC",
        "CW",
        "CWR",
        "Valence",
        "Arousal",
        "Dominance",
        "Ranking",
        "Region",
        "Sub-Region",
        "ISO Alpha-3" # Changed column name
      ),
      filter = "top"
    )
  })

  # Value boxes
  output$total_countries <- renderValueBox({
    valueBox(
      value = ifelse(is.null(values$data), 0, nrow(values$data)),
      subtitle = "Total Countries",
      icon = icon("globe"),
      color = "blue"
    )
  })

  output$total_clusters <- renderValueBox({
    valueBox(
      value = ifelse(is.null(values$data), 0, length(unique(
        values$data$cluster
      ))),
      subtitle = "Number of Clusters",
      icon = icon("layer-group"),
      color = "green"
    )
  })

  output$avg_words <- renderValueBox({
    valueBox(
      value = ifelse(is.null(values$data), 0,
        sprintf("%.2f", mean(values$data$wc, na.rm = TRUE))
      ),
      subtitle = "Avg. Words per Slogan",
      icon = icon("comment"),
      color = "yellow"
    )
  })

  # Geographic filters
  observeEvent(input$geo_region, {
    req(values$data)

    if (input$geo_region == "all") {
      subregions <- c("All" = "all", sort(unique(values$data$sub_region)))
    } else {
      subregions <- c("All" = "all", sort(unique(values$data$sub_region[values$data$region == input$geo_region])))
    }

    updateSelectInput(session, "geo_subregion", choices = subregions)
  })

  # Filter data based on geographic selections
  get_filtered_data <- reactive({
    req(values$data)

    filtered <- values$data

    # Apply cluster filter
    if (input$geo_cluster != "all") {
      filtered <- filtered %>% filter(cluster == input$geo_cluster)
    }

    # Apply region filter
    if (input$geo_region != "all") {
      filtered <- filtered %>% filter(region == input$geo_region)
    }

    # Apply sub-region filter
    if (input$geo_subregion != "all") {
      filtered <- filtered %>% filter(sub_region == input$geo_subregion)
    }

    return(filtered)
  })

  # World map plot
  # World map plot - ENHANCED DEBUGGING VERSION
  output$world_map <- renderPlotly({
    req(values$world_map, values$data)

    filtered_data <- get_filtered_data()

    # Enhanced debugging: Print all French overseas territories in your data
    french_overseas_data <- filtered_data %>%
      filter(iso_alpha3 %in% c("GUF", "GLP", "MTQ", "REU"))

    cat("=== French Overseas Territories in Your Data ===\n")
    if (nrow(french_overseas_data) > 0) {
      print(french_overseas_data[, c("country", "slogan", "iso_alpha3", "cluster")])
    } else {
      cat("No French overseas territories found in filtered data\n")
    }
    cat("=============================================\n")

    # Check what's in the world map for these territories
    french_map_features <- values$world_map %>%
      filter(iso_a3 %in% c("GUF", "GLP", "MTQ", "REU")) %>%
      select(name, map_unit, iso_a3)

    cat("=== French Overseas Territories in World Map ===\n")
    if (nrow(french_map_features) > 0) {
      print(french_map_features)
    } else {
      cat("No French overseas territories found in world map\n")
    }
    cat("============================================\n")

    # More flexible territory mapping - using multiple possible names
    territory_patterns <- list(
      "GUF" = c("French Guiana", "Guyane", "Guyane Française", "Guyane FranÃ§aise"),
      "GLP" = c("Guadeloupe"),
      "MTQ" = c("Martinique"),
      "REU" = c("Réunion", "RÃ©union", "Reunion")
    )

    # Start with basic join
    map_data <- values$world_map %>%
      left_join(filtered_data, by = c("iso_a3" = "iso_alpha3"))

    # Now handle each territory individually with flexible matching
    for (territory_iso in names(territory_patterns)) {
      territory_names <- territory_patterns[[territory_iso]]

      # Find territory data using multiple search strategies
      territory_data <- NULL

      # Strategy 1: Exact ISO match
      iso_matches <- filtered_data[filtered_data$iso_alpha3 == territory_iso, ]
      if (nrow(iso_matches) > 0) {
        territory_data <- iso_matches[1, ]
        cat("✓ Found", territory_iso, "by ISO match:", territory_data$country, "\n")
      } else {
        # Strategy 2: Partial name matching
        for (name_pattern in territory_names) {
          name_matches <- filtered_data[grepl(name_pattern, filtered_data$country, ignore.case = TRUE), ]
          if (nrow(name_matches) > 0) {
            territory_data <- name_matches[1, ]
            cat("✓ Found", territory_iso, "by name match:", territory_data$country, "matched to", name_pattern, "\n")
            break
          }
        }
      }

      # If we found the data, update the map
      if (!is.null(territory_data)) {
        # Find map features for this territory
        map_indices <- which(map_data$iso_a3 == territory_iso)

        if (length(map_indices) > 0) {
          # Update with territory-specific data
          map_data$country[map_indices] <- territory_data$country
          map_data$slogan[map_indices] <- territory_data$slogan
          map_data$cluster[map_indices] <- territory_data$cluster
          map_data$valence[map_indices] <- territory_data$valence
          map_data$arousal[map_indices] <- territory_data$arousal
          map_data$dominance[map_indices] <- territory_data$dominance
          map_data$ranking[map_indices] <- territory_data$ranking

          cat("✓ Updated map features for", territory_iso, ":", length(map_indices), "features\n")
        } else {
          cat("✗ No map features found for", territory_iso, "\n")
        }
      } else {
        cat("✗ No data found for", territory_iso, "\n")
      }
    }

    # Create hover text and cluster factors
    map_data <- map_data %>%
      mutate(
        cluster_factor = factor(cluster, levels = c(1, 2, 3)),
        hover_text = ifelse(
          is.na(slogan) | is.na(country),
          paste0(
            "<b>", map_unit, "</b><br>",
            "ISO: ", iso_a3, "<br>",
            "No tourism data available"
          ),
          paste0(
            "<b>", map_unit, "</b><br>",
            "Slogan: ", slogan, "<br>",
            "Valence: ", round(valence, 2), "<br>",
            "Arousal: ", round(arousal, 2), "<br>",
            "Dominance: ", round(dominance, 2), "<br>",
            "Ranking: ", ifelse(is.na(ranking), "N/A", ranking)
          )
        )
      )

    # Final check - what do our French territories look like now?
    final_french_check <- map_data %>%
      filter(iso_a3 %in% c("GUF", "GLP", "MTQ", "REU")) %>%
      select(map_unit, country, slogan, cluster, iso_a3)

    cat("=== Final French Territory Status ===\n")
    print(final_french_check)
    cat("====================================\n")

    # Create the plot
    p <- ggplot(map_data) +
      geom_sf(
        aes(fill = cluster_factor, text = hover_text),
        color = "white",
        size = 0.2
      ) +
      scale_fill_manual(
        values = c(
          "1" = "#ff7f0e",
          "2" = "#2ca02c",
          "3" = "#0000ff"
        ),
        labels = c("Cluster 1", "Cluster 2", "Cluster 3"),
        na.value = "#CCCCCC",
        name = "Cluster"
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom"
      )

    # Convert to plotly
    plotly_obj <- ggplotly(p, tooltip = "text", height = 650) %>%
      layout(
        legend = list(
          orientation = "h",
          y = -0.1,
          x = 0.5,
          xanchor = "center"
        ),
        margin = list(l = 10, r = 10, b = 10, t = 10, pad = 4)
      )

    # Apply hover styling
    for (i in seq_along(plotly_obj$x$data)) {
      trace <- plotly_obj$x$data[[i]]

      if (!is.null(trace$name)) {
        if (grepl("1", trace$name)) {
          plotly_obj$x$data[[i]]$hoverlabel <- list(
            bgcolor = "#ff7f0e",
            bordercolor = "white",
            font = list(color = "white")
          )
        } else if (grepl("2", trace$name)) {
          plotly_obj$x$data[[i]]$hoverlabel <- list(
            bgcolor = "#2ca02c",
            bordercolor = "white",
            font = list(color = "white")
          )
        } else if (grepl("3", trace$name)) {
          plotly_obj$x$data[[i]]$hoverlabel <- list(
            bgcolor = "#0000ff",
            bordercolor = "white",
            font = list(color = "white")
          )
        } else {
          plotly_obj$x$data[[i]]$hoverlabel <- list(
            bgcolor = "#CCCCCC",
            bordercolor = "white",
            font = list(color = "black")
          )
        }
      }
    }

    return(plotly_obj)
  })


  # Update the cluster summary table
  output$cluster_summary_table <- DT::renderDataTable({
    req(values$data)
    filtered_data <- get_filtered_data()

    summary_stats <- filtered_data %>%
      group_by(cluster) %>%
      summarise(
        `Cluster Size` = n(),
        `Avg Word Count` = round(mean(wc, na.rm = TRUE), 2),
        `Avg Valence` = round(mean(valence, na.rm = TRUE), 2),
        `Avg Arousal` = round(mean(arousal, na.rm = TRUE), 2),
        `Avg Dominance` = round(mean(dominance, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      rename(Cluster = cluster)

    rep_slogans <- calculate_representative_slogans(filtered_data)
    if (!is.null(rep_slogans)) {
      summary_stats <- summary_stats %>%
        left_join(
          rep_slogans %>% rename(Cluster = cluster),
          by = "Cluster"
        ) %>%
        rename(
          `Representative Slogan` = representative_slogan,
          `2nd Representative Slogan` = second_closest_slogan,
          `3rd Representative Slogan` = third_closest_slogan
        )
    }

    DT::datatable(
      summary_stats,
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(width = "120px", targets = 0), # Cluster
          list(width = "80px", targets = 1:4), # Stats
          list(width = "250px", targets = 5:7) # Slogans
        ),
        scrollX = TRUE
      ),
      rownames = FALSE,
      selection = "none"
    ) %>%
      DT::formatStyle(
        "Cluster",
        backgroundColor = styleEqual(
          c(1, 2, 3),
          c("#ff7f0e", "#2ca02c", "#0000ff")
        ),
        color = "white"
      )
  })

  # Selected countries table
  output$selected_countries <- DT::renderDataTable({
    filtered_data <- get_filtered_data()

    if (nrow(filtered_data) == 0) {
      return(DT::datatable(
        data.frame(Message = "No countries match the selected criteria")
      ))
    }

    DT::datatable(
      filtered_data %>%
        select(
          country,
          slogan,
          cluster,
          region,
          sub_region,
          wc,
          cw,
          cwr,
          valence,
          arousal,
          dominance,
          ranking
        ),
      options = list(scrollX = TRUE, pageLength = 10),
      colnames = c(
        "Country",
        "Slogan",
        "Cluster",
        "Region",
        "Sub-Region",
        "WC",
        "CW",
        "CWR",
        "Valence",
        "Arousal",
        "Dominance",
        "Ranking"
      )
    )
  })

  # Add ISO matching diagnostics table (updated version)
  output$iso_matching_diagnostics <- DT::renderDataTable(
    {
      req(values$data, values$world_map)

      # Create diagnostics using Alpha-3
      data_iso <- unique(values$data[, c("country", "iso_alpha3")])
      map_iso <- unique(values$world_map[, c("map_unit", "iso_a3")])

      # Define French overseas territories (Alpha-3)
      french_overseas <- c("GUF", "GLP", "MTQ", "REU")

      # Create matching report
      diagnostics <- data_iso %>%
        mutate(
          matched = iso_alpha3 %in% map_iso$iso_a3,
          map_unit_name = case_when(
            # Check if it's a French overseas territory first
            iso_alpha3 %in% french_overseas ~ "🇫🇷 Overseas France",
            # Then check if it's matched in the regular way
            matched ~ map_iso$map_unit[match(iso_alpha3, map_iso$iso_a3)],
            # Otherwise, not found
            TRUE ~ "Not found"
          ),
          status = case_when(
            iso_alpha3 %in% french_overseas ~ "🇫🇷 Overseas France",
            matched ~ "✓ Matched",
            TRUE ~ "✗ Unmatched"
          )
        ) %>%
        arrange(desc(iso_alpha3 %in% french_overseas), matched, country) %>%
        select(status, country, iso_alpha3, map_unit_name) %>%
        rename(
          "Status" = status,
          "Your Country Name" = country,
          "ISO Alpha-3" = iso_alpha3, # Changed column name
          "Map Unit Name" = map_unit_name
        )

      DT::datatable(
        diagnostics,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          columnDefs = list(
            list(width = "120px", targets = 0),
            list(width = "200px", targets = 1:3)
          )
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Status",
          backgroundColor = styleEqual(
            c("✓ Matched", "✗ Unmatched", "🇫🇷 Overseas France"),
            c("#d4edda", "#f8d7da", "#f8d7da") # Light blue for overseas territories
          )
        )
    },
    options = list(pageLength = 20)
  )

  # Cluster statistics
  output$cluster_stats <- DT::renderDataTable({
    req(values$data)

    stats <- values$data %>%
      group_by(cluster) %>%
      summarise(
        Size = n(),
        Avg_WC = round(mean(wc, na.rm = TRUE), 2),
        Avg_CW = round(mean(cw, na.rm = TRUE), 2),
        Avg_CWR = round(mean(cwr, na.rm = TRUE), 2),
        Avg_Valence = round(mean(valence, na.rm = TRUE), 2),
        Avg_Arousal = round(mean(arousal, na.rm = TRUE), 2),
        Avg_Dominance = round(mean(dominance, na.rm = TRUE), 2),
        .groups = "drop"
      )

    rep_slogans <- calculate_representative_slogans(values$data)
    if (!is.null(rep_slogans)) {
      stats <- stats %>%
        left_join(rep_slogans %>% rename(cluster = cluster), by = "cluster") %>%
        mutate(
          Representative_Slogans = paste(
            representative_slogan,
            second_closest_slogan,
            third_closest_slogan,
            sep = "; "
          )
        ) %>%
        select(-representative_slogan, -second_closest_slogan, -third_closest_slogan)
    } else {
      stats$Representative_Slogans <- NA
    }

    DT::datatable(stats, options = list(scrollX = TRUE))
  })

  # 3D Cluster plot
  output$cluster_3d_plot <- renderPlotly({
    req(values$data)

    plot_data <- values$data %>%
      filter(!is.na(valence) & !is.na(arousal) & !is.na(dominance)) %>%
      mutate(
        cluster_factor = as.factor(cluster),
        hover_text = paste0(
          "<b>", country, "</b><br>",
          "Slogan: ", slogan, "<br>",
          "Cluster: ", cluster, "<br>",
          "Valence: ", round(valence, 2), "<br>",
          "Arousal: ", round(arousal, 2), "<br>",
          "Dominance: ", round(dominance, 2), "<br>",
          "Ranking: ", ifelse(!is.na(ranking), ranking, "N/A")
        )
      )

    if (nrow(plot_data) == 0) {
      return(plotly_empty() %>% layout(title = "No data available for 3D plot"))
    }

    # Calculate centroids (means)
    centroids <- plot_data %>%
      group_by(cluster) %>%
      summarise(
        cent_valence = mean(valence),
        cent_arousal = mean(arousal),
        cent_dominance = mean(dominance),
        .groups = "drop"
      )

    # Find medoids (most representative actual points)
    medoids_data <- NULL
    if (!is.null(values$representative_slogans)) {
      # Use the representative slogans as medoids
      medoids_data <- plot_data %>%
        inner_join(
          values$representative_slogans %>%
            select(cluster, representative_slogan),
          by = c("cluster", "slogan" = "representative_slogan")
        ) %>%
        mutate(
          medoid_text = paste0(
            "<b>MEDOID - ", country, "</b><br>",
            "Representative Slogan: ", slogan, "<br>",
            "Cluster: ", cluster, "<br>",
            "Valence: ", round(valence, 2), "<br>",
            "Arousal: ", round(arousal, 2), "<br>",
            "Dominance: ", round(dominance, 2)
          )
        )
    }

    p <- plot_ly()

    # Add individual points
    for (cluster_num in sort(unique(plot_data$cluster))) {
      cluster_data <- plot_data %>% filter(cluster == cluster_num)

      p <- p %>% add_trace(
        data = cluster_data,
        x = ~valence, y = ~arousal, z = ~dominance,
        type = "scatter3d", mode = "markers",
        name = paste("Cluster", cluster_num),
        marker = list(
          color = cluster_colors[as.character(cluster_num)],
          size = 6,
          opacity = 0.6,
          line = list(color = "white", width = 0.5)
        ),
        text = ~hover_text,
        hovertemplate = "%{text}<extra></extra>",
        showlegend = TRUE
      )
    }

    # Add centroids if requested
    if (input$center_type %in% c("centroids", "both")) {
      for (cluster_num in sort(unique(centroids$cluster))) {
        centroid_data <- centroids %>% filter(cluster == cluster_num)

        p <- p %>% add_trace(
          data = centroid_data,
          x = ~cent_valence, y = ~cent_arousal, z = ~cent_dominance,
          type = "scatter3d", mode = "markers",
          name = paste("Centroid", cluster_num),
          marker = list(
            color = cluster_colors[as.character(cluster_num)],
            size = 15,
            opacity = 1,
            symbol = "diamond",
            line = list(color = "black", width = 3)
          ),
          hovertemplate = paste0(
            "<b>Centroid (Mean) - Cluster ", cluster_num, "</b><br>",
            "Valence: %{x:.2f}<br>",
            "Arousal: %{y:.2f}<br>",
            "Dominance: %{z:.2f}<extra></extra>"
          ),
          showlegend = TRUE
        )
      }
    }

    # Add medoids if requested and available
    if (input$center_type %in% c("medoids", "both") && !is.null(medoids_data)) {
      for (cluster_num in sort(unique(medoids_data$cluster))) {
        medoid_data <- medoids_data %>% filter(cluster == cluster_num)

        # Create proper hover text
        medoid_hover_text <- paste0(
          "<b>MEDOID - ", medoid_data$country, "</b><br>",
          "Representative Slogan: ", medoid_data$slogan, "<br>",
          "Cluster: ", cluster_num, "<br>",
          "Valence: ", round(medoid_data$valence, 2), "<br>",
          "Arousal: ", round(medoid_data$arousal, 2), "<br>",
          "Dominance: ", round(medoid_data$dominance, 2)
        )

        p <- p %>% add_trace(
          x = medoid_data$valence,
          y = medoid_data$arousal,
          z = medoid_data$dominance,
          type = "scatter3d",
          mode = "markers",
          name = paste("Medoid", cluster_num),
          marker = list(
            color = cluster_colors[as.character(cluster_num)],
            size = 18,
            opacity = 1,
            symbol = "cross",
            line = list(color = "white", width = 3)
          ),
          text = medoid_hover_text,
          hoverinfo = "text",
          hovertemplate = paste0(
            "<b>MEDOID - ", medoid_data$country, "</b><br>",
            "Representative Slogan: ", medoid_data$slogan, "<br>",
            "Cluster: ", cluster_num, "<br>",
            "Valence: %{x:.2f}<br>",
            "Arousal: %{y:.2f}<br>",
            "Dominance: %{z:.2f}<extra></extra>"
          ),
          showlegend = TRUE
        )
      }
    }

    title_text <- case_when(
      input$center_type == "centroids" ~ "3D Cluster Plot with Centroids",
      input$center_type == "medoids" ~ "3D Cluster Plot with Medoids",
      input$center_type == "both" ~ "3D Cluster Plot with Centroids & Medoids",
      TRUE ~ "3D Cluster Analysis"
    )

    p %>%
      layout(
        title = title_text,
        scene = list(
          xaxis = list(title = "Valence"),
          yaxis = list(title = "Arousal"),
          zaxis = list(title = "Dominance"),
          camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
        ),
        legend = list(
          orientation = "v",
          x = 1.02,
          y = 1
        )
      ) %>%
      config(
        toImageButtonOptions = list(
          format = "png",
          filename = "3D_cluster_plot",
          height = 1200, # Height in pixels (for 300 DPI at ~4 inches)
          width = 1600, # Width in pixels (for 300 DPI at ~5.33 inches)
          scale = 3 # Scale factor (1 = native resolution)
        ),
        displayModeBar = TRUE,
        modeBarButtonsToAdd = list("downloadPlot")
      )
  })

  # Valence vs Arousal plot
  output$valence_arousal_plot <- renderPlotly({
    req(values$data)

    # Filter out rows with missing values - same as 3D plot
    plot_data <- values$data %>%
      filter(!is.na(valence) & !is.na(arousal) & !is.na(cwr)) %>%
      mutate(
        # Use the SAME hover text format as the 3D plot
        hover_text = paste0(
          "<b>",
          country,
          "</b><br>",
          "Slogan: ",
          slogan,
          "<br>",
          "Cluster: ",
          cluster,
          "<br>",
          "Valence: ",
          round(valence, 2),
          "<br>",
          "Arousal: ",
          round(arousal, 2),
          "<br>",
          "CWR: ",
          round(cwr, 2)
        )
      )

    if (nrow(plot_data) == 0) {
      return(plotly_empty() %>%
        layout(title = "No data available for plot"))
    }

    # Define cluster colors
    cluster_colors <- c("1" = "#ff7f0e", "2" = "#2ca02c", "3" = "#0000ff")

    # Create empty plot
    p <- plot_ly()

    # Add each cluster as a separate trace to ensure color control
    for (cluster_num in sort(unique(plot_data$cluster))) {
      cluster_data <- plot_data %>% filter(cluster == cluster_num)

      p <- p %>% add_trace(
        data = cluster_data,
        x = ~valence,
        y = ~arousal,
        type = "scatter",
        mode = "markers",
        name = paste("Cluster", cluster_num),
        marker = list(
          color = cluster_colors[as.character(cluster_num)],
          size = 8,
          opacity = 0.8
        ),
        text = ~hover_text,
        hovertemplate = "%{text}<extra></extra>",
        hoverlabel = list(
          bgcolor = cluster_colors[as.character(cluster_num)],
          bordercolor = "white",
          font = list(color = "white")
        )
      )
    }

    # Layout
    p <- p %>% layout(
      title = "2D Cluster Plot",
      xaxis = list(title = "Valence"),
      yaxis = list(title = "Arousal"),
      legend = list(
        title = list(text = "Cluster"),
        orientation = "v"
      )
    )

    return(p)
  })

  # Word frequency analysis
  get_frequency_data <- reactive({
    req(values$corpus, input$selected_cluster)

    if (input$selected_cluster == "all") {
      corpus_subset <- values$corpus
      group_label <- "All Clusters"
    } else {
      cluster_num <- as.numeric(gsub("Cluster ", "", input$selected_cluster))
      corpus_subset <- corpus_subset(values$corpus, cluster == cluster_num)
      group_label <- paste("Cluster", cluster_num)
    }

    tokens_subset <- tokens(corpus_subset,
      remove_punct = TRUE,
      remove_symbols = TRUE
    )

    if (input$remove_stopwords) {
      tokens_subset <- tokens_remove(tokens_subset, stopwords("english"))
    }

    dfm_subset <- dfm(tokens_subset)
    freq_data <- textstat_frequency(dfm_subset, n = input$top_n_words)

    # Add cluster information to the frequency data
    freq_data$group <- group_label

    return(freq_data)
  })

  output$frequency_plot <- renderPlotly({
    freq_data <- get_frequency_data()

    p <- ggplot(freq_data, aes(x = reorder(feature, frequency), y = frequency)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = paste(
          "Top",
          input$top_n_words,
          "Words in",
          unique(freq_data$group)
        ),
        x = "Words",
        y = "Frequency"
      ) +
      theme_minimal()

    ggplotly(p)
  })

  output$frequency_table <- DT::renderDataTable({
    freq_data <- get_frequency_data()
    # Reorder columns to show group first
    freq_data <- freq_data %>%
      select(group, feature, frequency, docfreq) %>%
      rename(
        "Group" = group,
        "Word" = feature,
        "Frequency" = frequency,
        "Document Frequency" = docfreq
      )

    DT::datatable(freq_data, options = list(pageLength = 15))
  })

  get_keyness_data <- reactive({
    req(values$corpus)

    tryCatch(
      {
        # Check if corpus has valid data
        if (ndoc(values$corpus) == 0) {
          return(data.frame(
            feature = character(0),
            chi2 = numeric(0),
            p = numeric(0),
            cluster = character(0)
          ))
        }

        # Create tokens with better error handling
        tokens_all <- tokens(values$corpus,
          remove_punct = TRUE,
          remove_symbols = TRUE,
          remove_numbers = TRUE
        ) %>%
          tokens_remove(stopwords("english")) %>%
          tokens_remove("") # Remove empty tokens

        # Check if we have any tokens left
        if (sum(ntoken(tokens_all)) == 0) {
          return(data.frame(
            feature = character(0),
            chi2 = numeric(0),
            p = numeric(0),
            cluster = character(0)
          ))
        }

        # Create DFM
        dfm_all <- dfm(tokens_all)

        # Remove features that appear in less than 2 documents (optional)
        dfm_all <- dfm_trim(dfm_all, min_docfreq = 2)

        # Check if DFM has any features
        if (nfeat(dfm_all) == 0) {
          return(data.frame(
            feature = character(0),
            chi2 = numeric(0),
            p = numeric(0),
            cluster = character(0)
          ))
        }

        # Group by cluster
        dfm_grouped <- dfm_group(dfm_all, groups = docvars(values$corpus, "cluster"))

        # Get unique clusters
        clusters <- unique(docvars(values$corpus, "cluster"))
        clusters <- clusters[!is.na(clusters)]

        if (length(clusters) == 0) {
          return(data.frame(
            feature = character(0),
            chi2 = numeric(0),
            p = numeric(0),
            cluster = character(0)
          ))
        }

        # Calculate keyness for each cluster
        keyness_results <- list()

        for (i in seq_along(clusters)) {
          cluster_val <- clusters[i]

          tryCatch(
            {
              # Calculate keyness with error handling
              keyness_res <- textstat_keyness(
                dfm_grouped,
                target = as.character(cluster_val),
                correction = "yates"
              )

              # Add cluster information
              keyness_res$cluster <- paste("Cluster", cluster_val)
              keyness_results[[i]] <- keyness_res
            },
            error = function(e) {
              message("Error calculating keyness for cluster ", cluster_val, ": ", e$message)
              # Return empty result for this cluster
              keyness_results[[i]] <<- data.frame(
                feature = character(0),
                chi2 = numeric(0),
                p = numeric(0),
                cluster = character(0)
              )
            }
          )
        }

        # Combine results
        if (length(keyness_results) > 0) {
          # Filter out NULL or empty results
          valid_results <- keyness_results[!sapply(keyness_results, function(x) is.null(x) || nrow(x) == 0)]

          if (length(valid_results) > 0) {
            return(bind_rows(valid_results))
          }
        }

        # Return empty data frame if no valid results
        return(data.frame(
          feature = character(0),
          chi2 = numeric(0),
          p = numeric(0),
          cluster = character(0)
        ))
      },
      error = function(e) {
        message("Error in get_keyness_data: ", e$message)
        return(data.frame(
          feature = character(0),
          chi2 = numeric(0),
          p = numeric(0),
          cluster = character(0)
        ))
      }
    )
  })

  # Word clouds
  create_wordcloud_data <- function(cluster_num) {
    req(values$corpus)

    corpus_cluster <- corpus_subset(values$corpus, cluster == cluster_num)
    tokens_cluster <- tokens(corpus_cluster,
      remove_punct = TRUE,
      remove_symbols = TRUE
    )

    if (input$remove_common) {
      tokens_cluster <- tokens_remove(tokens_cluster, stopwords("english"))
    }

    dfm_cluster <- dfm(tokens_cluster)
    return(dfm_cluster)
  }

  output$wordcloud1 <- renderPlot({
    req(values$processed)

    dfm_data <- create_wordcloud_data(1)
    if (sum(dfm_data) > 0) {
      textplot_wordcloud(dfm_data,
        max_words = input$max_words,
        color = cluster_colors["1"]
      )
    }
  })

  output$wordcloud2 <- renderPlot({
    req(values$processed)

    dfm_data <- create_wordcloud_data(2)
    if (sum(dfm_data) > 0) {
      textplot_wordcloud(dfm_data,
        max_words = input$max_words,
        color = cluster_colors["2"]
      )
    }
  })

  output$wordcloud3 <- renderPlot({
    req(values$processed)

    dfm_data <- create_wordcloud_data(3)
    if (sum(dfm_data) > 0) {
      textplot_wordcloud(dfm_data,
        max_words = input$max_words,
        color = cluster_colors["3"]
      )
    }
  })

  # Keyness analysis
  output$keyness_plot <- renderPlotly({
    keyness_data <- get_keyness_data()

    # Check if we have valid data
    if (is.null(keyness_data) || nrow(keyness_data) == 0) {
      p <- ggplot() +
        annotate("text",
          x = 0.5, y = 0.5, label = "No distinctive words found.\nThis may occur if:\n- Data is too sparse\n- All clusters have similar vocabulary\n- Text preprocessing removed too many words",
          hjust = 0.5, vjust = 0.5, size = 4
        ) +
        theme_void() +
        labs(title = "Distinctive Words Analysis")

      return(ggplotly(p))
    }

    # Filter for significant results
    significant_data <- keyness_data %>%
      filter(abs(chi2) > 2) %>% # Only keep significant results
      group_by(cluster) %>%
      slice_max(abs(chi2), n = 10) %>%
      ungroup()

    if (nrow(significant_data) == 0) {
      p <- ggplot() +
        annotate("text",
          x = 0.5, y = 0.5, label = "No significantly distinctive words found.\nTry adjusting the significance threshold or\ncheck if clusters have sufficient vocabulary differences.",
          hjust = 0.5, vjust = 0.5, size = 4
        ) +
        theme_void() +
        labs(title = "Distinctive Words Analysis")

      return(ggplotly(p))
    }

    # Create named color vector matching cluster numbers
    cluster_fill <- c(
      "Cluster 1" = "#ff7f0e",
      "Cluster 2" = "#2ca02c",
      "Cluster 3" = "#0000ff"
    )

    p <- ggplot(
      significant_data,
      aes(
        x = reorder(feature, chi2),
        y = chi2,
        fill = cluster,
        text = paste0(
          "Word: ", feature, "\n",
          "Cluster: ", cluster, "\n",
          "χ²: ", round(chi2, 2), "\n",
          "p-value: ", format.pval(p, digits = 2)
        )
      )
    ) +
      geom_col() +
      scale_fill_manual(values = cluster_fill) +
      facet_wrap(~cluster, scales = "free_y") +
      coord_flip() +
      labs(
        title = "Most Distinctive Words by Cluster (Yates-corrected χ²)",
        x = "Word",
        y = "Keyness Score"
      ) +
      theme_minimal() +
      theme(legend.position = "none")

    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })

  # Thematic analysis
  output$thematic_plot <- renderPlotly({
    req(values$data)

    # Use the same logic as in the CSV export for thematic columns
    thematic_data <- values$data %>%
      mutate(
        nature = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$nature, collapse = "|")
        )),
        culture = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$culture, collapse = "|")
        )),
        urban = as.numeric(str_detect(
          tolower(slogan), paste(tourism_dict$urban, collapse = "|")
        )),
        adventure = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$adventure, collapse = "|")
        )),
        relaxation = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$relaxation, collapse = "|")
        )),
        luxury = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$luxury, collapse = "|")
        )),
        friendliness = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$friendliness, collapse = "|")
        )),
        romantic = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$romantic, collapse = "|")
        )),
        geographical = as.numeric((
          str_detect(tolower(slogan), "\\bheart\\b") &
            !str_detect(
              tolower(slogan),
              "\\byour heart\\b|\\bwarm heart\\b|\\bfrom the heart\\b"
            )
        ) |
          str_detect(
            tolower(slogan), paste(
              c(
                "\\bcenter\\b",
                "crossroad*",
                "\\bgateway to\\b"
              ),
              collapse = "|"
            )
          )),
        positive_emotion = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$positive_emotion, collapse = "|")
        )),
        excitement = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$excitement, collapse = "|")
        )),
        authenticity = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$authenticity, collapse = "|")
        )),
        action_verbs = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$action_verbs, collapse = "|")
        )),
        timeless = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$timeless, collapse = "|")
        )),
        seasonal = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$seasonal, collapse = "|")
        )),
        uniqueness = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$uniqueness, collapse = "|")
        )),
        superlatives = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$superlatives, collapse = "|")
        ))
      )

    # Calculate percentage for each theme within each cluster
    thematic_long <- thematic_data %>%
      select(
        cluster,
        nature,
        culture,
        urban,
        geographical,
        adventure,
        relaxation,
        luxury,
        friendliness,
        romantic,
        positive_emotion,
        excitement,
        authenticity,
        action_verbs,
        uniqueness,
        superlatives
      ) %>%
      pivot_longer(-cluster, names_to = "Theme", values_to = "Value") %>%
      group_by(cluster, Theme) %>%
      summarise(Percentage = mean(Value) * 100, .groups = "drop") %>%
      mutate(Cluster = paste("Cluster", cluster))

    # Order themes by total percentage
    theme_order <- thematic_long %>%
      group_by(Theme) %>%
      summarise(total = sum(Percentage)) %>%
      arrange(desc(total)) %>%
      pull(Theme)

    thematic_long$Theme <- factor(thematic_long$Theme, levels = theme_order)

    # Define cluster colors for the plot
    cluster_fill <- c(
      "Cluster 1" = "#ff7f0e",
      "Cluster 2" = "#2ca02c",
      "Cluster 3" = "#0000ff"
    )

    p <- ggplot(thematic_long, aes(x = Theme, y = Percentage, fill = Cluster)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = cluster_fill) +
      labs(title = "Thematic Distribution by Cluster", x = "Themes", y = "Percentage (%)") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
      )

    ggplotly(p) %>%
      layout(legend = list(
        orientation = "h",
        x = 0.5,
        y = 1.1,
        xanchor = "center",
        yanchor = "bottom"
      ))
  })

  # Linguistic features
  get_linguistic_data <- function() {
    req(values$data)

    linguistic_data <- values$data %>%
      mutate(
        # Basic metrics
        word_count = wc,
        content_words = cw,
        content_word_ratio = cwr,

        # Other calculations
        char_count = nchar(slogan),
        avg_word_length = ifelse(wc > 0, char_count / wc, 0),

        # Structural features - using improved detection
        has_imperative = as.numeric(str_detect(
          slogan, linguistic_patterns$imperative
        )),
        has_interrogative = as.numeric(str_detect(
          slogan, linguistic_patterns$interrogative
        )),
        has_rhyme = as.numeric(sapply(slogan, detect_rhyming)),
        has_alliteration = as.numeric(sapply(slogan, detect_alliteration)),
        has_personification = as.numeric(sapply(slogan, detect_personification)),

        # Semantic features
        has_sensory = as.numeric(str_detect(
          tolower(slogan), linguistic_patterns$sensory_words
        )),
        has_metaphor = as.numeric(str_detect(
          tolower(slogan), linguistic_patterns$metaphorical
        )),
        has_simile = as.numeric(sapply(slogan, detect_simile)),
        has_hyperbole = as.numeric(str_detect(
          tolower(slogan), linguistic_patterns$hyperbolic
        )),


        # New linguistic markers
        has_local_lang = as.numeric(sapply(slogan, detect_local_language)),
        has_indigenous = as.numeric(
          str_detect(tolower(slogan), linguistic_patterns$indigenous_terms)
        ),
        has_inclusive = as.numeric(str_detect(
          tolower(slogan), linguistic_patterns$inclusive_we
        )),
        has_direct = as.numeric(str_detect(
          tolower(slogan), linguistic_patterns$direct_you
        )),
        has_submissive = as.numeric(str_detect(
          tolower(slogan), linguistic_patterns$submissive_her
        ))
      )

    return(linguistic_data)
  }

  # Get thematic pattern data
  get_thematic_pattern_data <- reactive({
    req(values$data, input$selected_theme)

    thematic_data <- values$data %>%
      mutate(
        nature = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$nature, collapse = "|")
        )),
        culture = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$culture, collapse = "|")
        )),
        urban = as.numeric(str_detect(
          tolower(slogan), paste(tourism_dict$urban, collapse = "|")
        )),
        adventure = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$adventure, collapse = "|")
        )),
        relaxation = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$relaxation, collapse = "|")
        )),
        luxury = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$luxury, collapse = "|")
        )),
        friendliness = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$friendliness, collapse = "|")
        )),
        romantic = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$romantic, collapse = "|")
        )),
        geographical = as.numeric((
          str_detect(tolower(slogan), "\\bheart\\b") &
            !str_detect(
              tolower(slogan),
              "\\byour heart\\b|\\bwarm heart\\b|\\bfrom the heart\\b"
            )
        ) |
          str_detect(
            tolower(slogan), paste(
              c(
                "\\bcenter\\b",
                "crossroad*",
                "\\bgateway to\\b"
              ),
              collapse = "|"
            )
          )),
        positive_emotion = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$positive_emotion, collapse = "|")
        )),
        excitement = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$excitement, collapse = "|")
        )),
        authenticity = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$authenticity, collapse = "|")
        )),
        action_verbs = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$action_verbs, collapse = "|")
        )),
        uniqueness = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$uniqueness, collapse = "|")
        )),
        superlatives = as.numeric(str_detect(
          tolower(slogan),
          paste(tourism_dict$superlatives, collapse = "|")
        ))
      )

    # Filter for selected theme
    selected_col <- sym(input$selected_theme)

    filtered_data <- thematic_data %>%
      filter(!!selected_col == 1) %>%
      select(country, slogan, cluster) %>%
      arrange(cluster, country)

    return(filtered_data)
  })

  # Get linguistic pattern data
  get_linguistic_pattern_data <- reactive({
    req(values$data, input$selected_linguistic)

    linguistic_data <- get_linguistic_data()

    # Filter for selected linguistic feature
    selected_col <- sym(input$selected_linguistic)

    filtered_data <- linguistic_data %>%
      filter(!!selected_col == 1) %>%
      select(country, slogan, cluster) %>%
      arrange(cluster, country)

    return(filtered_data)
  })

  # Structural features plot
  output$structural_plot <- renderPlotly({
    linguistic_data <- get_linguistic_data()

    structural_summary <- linguistic_data %>%
      group_by(cluster) %>%
      summarise(
        Imperative = mean(has_imperative) * 100,
        Interrogative = mean(has_interrogative) * 100,
        Rhyme = mean(has_rhyme) * 100,
        Alliteration = mean(has_alliteration) * 100,
        Personification = mean(has_personification) * 100,
        .groups = "drop"
      ) %>%
      pivot_longer(-cluster, names_to = "Feature", values_to = "Percentage") %>%
      mutate(Cluster = paste("Cluster", cluster))

    p <- ggplot(
      structural_summary,
      aes(x = Feature, y = Percentage, fill = Cluster)
    ) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "Cluster 1" = "#ff7f0e",
        # Orange
        "Cluster 2" = "#2ca02c",
        # Green
        "Cluster 3" = "#0000ff"
      )) + # Blue
      labs(title = "Structural Feature Distribution", x = "Features", y = "Percentage (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p)
  })

  # Semantic features plot
  output$semantic_plot <- renderPlotly({
    linguistic_data <- get_linguistic_data()

    semantic_summary <- linguistic_data %>%
      group_by(cluster) %>%
      summarise(
        Sensory = mean(has_sensory) * 100,
        Metaphor = mean(has_metaphor) * 100,
        Simile = mean(has_simile) * 100,
        Hyperbole = mean(has_hyperbole) * 100,
        .groups = "drop"
      ) %>%
      pivot_longer(-cluster, names_to = "Feature", values_to = "Percentage") %>%
      mutate(Cluster = paste("Cluster", cluster))

    p <- ggplot(
      semantic_summary,
      aes(x = Feature, y = Percentage, fill = Cluster)
    ) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "Cluster 1" = "#ff7f0e",
        # Orange
        "Cluster 2" = "#2ca02c",
        # Green
        "Cluster 3" = "#0000ff"
      )) + # Blue
      labs(title = "Semantic Feature Distribution", x = "Features", y = "Percentage (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p)
  })

  output$markers_plot <- renderPlotly({
    linguistic_data <- get_linguistic_data()

    markers_summary <- linguistic_data %>%
      group_by(cluster) %>%
      summarise(
        Local_Language = mean(has_local_lang) * 100,
        Indigenous_Terms = mean(has_indigenous) * 100,
        Inclusive_We = mean(has_inclusive) * 100,
        Direct_You = mean(has_direct) * 100,
        Submissive_Her = mean(has_submissive) * 100,
        .groups = "drop"
      ) %>%
      pivot_longer(-cluster, names_to = "Feature", values_to = "Percentage") %>%
      mutate(Cluster = paste("Cluster", cluster))

    p <- ggplot(
      markers_summary,
      aes(x = Feature, y = Percentage, fill = Cluster)
    ) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "Cluster 1" = "#ff7f0e",
        "Cluster 2" = "#2ca02c",
        "Cluster 3" = "#0000ff"
      )) +
      labs(title = "Linguistic & Cultural Markers Distribution", x = "Markers", y = "Percentage (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p)
  })

  # Linguistic features table
  output$linguistic_features <- DT::renderDataTable({
    linguistic_data <- get_linguistic_data()

    feature_summary <- linguistic_data %>%
      group_by(cluster) %>%
      summarise(
        # Basic stats
        Avg_Word_Count = round(mean(wc), 2),
        Avg_Char_Count = round(mean(char_count), 2),
        Avg_Word_Length = round(mean(avg_word_length), 2),
        Avg_CWR = round(mean(cwr, na.rm = TRUE), 2),
        Avg_Valence = round(mean(valence, na.rm = TRUE), 2),
        Avg_Arousal = round(mean(arousal, na.rm = TRUE), 2),
        Avg_Dominance = round(mean(dominance, na.rm = TRUE), 2),

        # Structural features (%)
        Pct_Imperative = round(mean(has_imperative) * 100, 1),
        Pct_Interrogative = round(mean(has_interrogative) * 100, 1),
        Pct_Rhyme = round(mean(has_rhyme) * 100, 1),
        Pct_Alliteration = round(mean(has_alliteration) * 100, 1),
        Pct_Personification = round(mean(has_personification) * 100, 1),

        # Semantic features (%)
        Pct_Sensory = round(mean(has_sensory) * 100, 1),
        Pct_Metaphor = round(mean(has_metaphor) * 100, 1),
        Pct_Simile = round(mean(has_simile) * 100, 1),
        Pct_Hyperbole = round(mean(has_hyperbole) * 100, 1),

        # Linguistic markers (%)
        Pct_Local_Lang = round(mean(has_local_lang) * 100, 1),
        Pct_Indigenous = round(mean(has_indigenous) * 100, 1),
        Pct_Inclusive = round(mean(has_inclusive) * 100, 1),
        Pct_Direct = round(mean(has_direct) * 100, 1),
        Pct_Submissive = round(mean(has_submissive) * 100, 1),
      )

    DT::datatable(
      feature_summary,
      options = list(scrollX = TRUE, pageLength = 10),
      caption = "Comprehensive Linguistic Features by Cluster"
    )
  })

  # Thematic pattern table
  output$thematic_pattern_table <- DT::renderDataTable({
    pattern_data <- get_thematic_pattern_data()

    if (nrow(pattern_data) == 0) {
      return(DT::datatable(
        data.frame(Message = "No countries/territories contain this thematic pattern"),
        options = list(dom = "t", ordering = FALSE)
      ))
    }

    DT::datatable(
      pattern_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(width = "200px", targets = 0), # Country
          list(width = "400px", targets = 1), # Slogan
          list(width = "80px", targets = 2) # Cluster
        )
      ),
      colnames = c("Country/Territory", "Slogan", "Cluster"),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "cluster",
        backgroundColor = styleEqual(
          c(1, 2, 3),
          c("#ff7f0e", "#2ca02c", "#0000ff")
        ),
        color = "white"
      )
  })

  # Linguistic pattern table
  output$linguistic_pattern_table <- DT::renderDataTable({
    pattern_data <- get_linguistic_pattern_data()

    if (nrow(pattern_data) == 0) {
      return(DT::datatable(
        data.frame(Message = "No countries/territories contain this linguistic feature"),
        options = list(dom = "t", ordering = FALSE)
      ))
    }

    DT::datatable(
      pattern_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(width = "200px", targets = 0), # Country
          list(width = "400px", targets = 1), # Slogan
          list(width = "80px", targets = 2) # Cluster
        )
      ),
      colnames = c("Country/Territory", "Slogan", "Cluster"),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "cluster",
        backgroundColor = styleEqual(
          c(1, 2, 3),
          c("#ff7f0e", "#2ca02c", "#0000ff")
        ),
        color = "white"
      )
  })

  # Export linguistic feature breakdown
  output$download_linguistic_breakdown <- downloadHandler(
    filename = function() {
      paste("linguistic_feature_breakdown_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      linguistic_data <- get_linguistic_data() %>%
        select(
          cluster,
          country,
          slogan,
          has_imperative,
          has_interrogative,
          has_rhyme,
          has_alliteration,
          has_personification,
          has_sensory,
          has_metaphor,
          has_simile,
          has_hyperbole,
          has_local_lang,
          has_indigenous,
          has_inclusive,
          has_direct,
          has_submissive
        ) %>%
        rename(
          Cluster = cluster,
          Country = country,
          Slogan = slogan,
          Imperative = has_imperative,
          Interrogative = has_interrogative,
          Rhyme = has_rhyme,
          Alliteration = has_alliteration,
          Personification = has_personification,
          Sensory = has_sensory,
          Metaphor = has_metaphor,
          Simile = has_simile,
          Hyperbole = has_hyperbole,
          Local_Language = has_local_lang,
          Indigenous_Terms = has_indigenous,
          Inclusive_We = has_inclusive,
          Direct_You = has_direct,
          Submissive_Her = has_submissive
        )

      write.csv(linguistic_data, file, row.names = FALSE)
    }
  )

  # Export functionality
  output$download_summary <- downloadHandler(
    filename = function() {
      paste("cluster_summary_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$data)

      summary_stats <- values$data %>%
        group_by(cluster) %>%
        summarise(
          Size = n(),
          Avg_WC = round(mean(wc, na.rm = TRUE), 2),
          Avg_CW = round(mean(cw, na.rm = TRUE), 2),
          Avg_CWR = round(mean(cwr, na.rm = TRUE), 2),
          Avg_Valence = round(mean(valence, na.rm = TRUE), 2),
          Avg_Arousal = round(mean(arousal, na.rm = TRUE), 2),
          Avg_Dominance = round(mean(dominance, na.rm = TRUE), 2),
          .groups = "drop"
        )

      write.csv(summary_stats, file, row.names = FALSE)
    }
  )

  output$download_frequency <- downloadHandler(
    filename = function() {
      paste("word_frequencies_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      freq_data <- get_frequency_data()
      write.csv(freq_data, file, row.names = FALSE)
    }
  )

  output$download_keyness <- downloadHandler(
    filename = function() {
      paste("distinctive_words_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        get_keyness_data() %>%
          select(cluster, feature, chi2, p) %>%
          rename(
            Cluster = cluster,
            Word = feature,
            Keyness_Score = chi2,
            P_value = p
          ),
        file,
        row.names = FALSE
      )
    }
  )

  output$download_thematic <- downloadHandler(
    filename = function() {
      paste("thematic_distribution_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$data)
      thematic_data <- values$data %>%
        mutate(
          nature = as.numeric(str_detect(
            tolower(slogan),
            paste(tourism_dict$nature, collapse = "|")
          )),
          culture = as.numeric(str_detect(
            tolower(slogan),
            paste(tourism_dict$culture, collapse = "|")
          )),
          urban = as.numeric(str_detect(
            tolower(slogan), paste(tourism_dict$urban, collapse = "|")
          )),
          adventure = as.numeric(str_detect(
            tolower(slogan),
            paste(tourism_dict$adventure, collapse = "|")
          )),
          relaxation = as.numeric(str_detect(
            tolower(slogan),
            paste(tourism_dict$relaxation, collapse = "|")
          )),
          luxury = as.numeric(str_detect(
            tolower(slogan),
            paste(tourism_dict$luxury, collapse = "|")
          )),
          friendliness = as.numeric(str_detect(
            tolower(slogan),
            paste(tourism_dict$friendliness, collapse = "|")
          )),
          # Romantic: only the three phrases
          romantic = as.numeric(str_detect(
            tolower(slogan),
            paste(tourism_dict$romantic, collapse = "|")
          )),
          # Geographical: heart as a word, but not in the three phrases
          geographical = as.numeric((
            str_detect(tolower(slogan), "\\bheart\\b") &
              !str_detect(
                tolower(slogan),
                "\\byour heart\\b|\\bwarm heart\\b|\\bfrom the heart\\b"
              )
          ) |
            str_detect(
              tolower(slogan), paste(
                c(
                  "\\bcenter\\b",
                  "crossroad*",
                  "\\bgateway to\\b"
                ),
                collapse = "|"
              )
            )),
          positive_emotion = as.numeric(str_detect(
            tolower(slogan),
            paste(tourism_dict$positive_emotion, collapse = "|")
          )),
          excitement = as.numeric(str_detect(
            tolower(slogan),
            paste(tourism_dict$excitement, collapse = "|")
          )),
          authenticity = as.numeric(str_detect(
            tolower(slogan),
            paste(tourism_dict$authenticity, collapse = "|")
          )),
          action_verbs = as.numeric(str_detect(
            tolower(slogan),
            paste(tourism_dict$action_verbs, collapse = "|")
          )),
          timeless = as.numeric(str_detect(
            tolower(slogan),
            paste(tourism_dict$timeless, collapse = "|")
          )),
          seasonal = as.numeric(str_detect(
            tolower(slogan),
            paste(tourism_dict$seasonal, collapse = "|")
          )),
          uniqueness = as.numeric(str_detect(
            tolower(slogan),
            paste(tourism_dict$uniqueness, collapse = "|")
          )),
          superlatives = as.numeric(str_detect(
            tolower(slogan),
            paste(tourism_dict$superlatives, collapse = "|")
          ))
        ) %>%
        select(
          cluster,
          country,
          slogan,
          nature,
          culture,
          urban,
          geographical,
          adventure,
          relaxation,
          luxury,
          friendliness,
          romantic,
          positive_emotion,
          excitement,
          authenticity,
          action_verbs,
          uniqueness,
          superlatives,
          timeless,
          seasonal
        ) %>%
        rename(
          Cluster = cluster,
          Country = country,
          Slogan = slogan,
          Nature = nature,
          Culture = culture,
          Urban = urban,
          Geographic = geographical,
          Adventure = adventure,
          Relaxation = relaxation,
          Luxury = luxury,
          Friendliness = friendliness,
          Romantic = romantic,
          Positive_Emotion = positive_emotion,
          Excitement = excitement,
          Authenticity = authenticity,
          Action_Verbs = action_verbs,
          Timeless = timeless,
          Seasonal = seasonal,
          Uniqueness = uniqueness,
          Superlatives = superlatives
        )
      write.csv(thematic_data, file, row.names = FALSE)
    }
  )

  output$download_linguistic <- downloadHandler(
    filename = function() {
      paste("linguistic_features_summary", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$data)

      linguistic_data <- values$data %>%
        mutate(
          # Basic metrics
          word_count = wc,
          char_count = nchar(slogan),
          avg_word_length = ifelse(word_count > 0, char_count / word_count, 0),

          # Structural features
          has_imperative = str_detect(slogan, linguistic_patterns$imperative),
          has_interrogative = str_detect(slogan, linguistic_patterns$interrogative),
          has_rhyme = as.numeric(sapply(slogan, detect_rhyming)),
          has_alliteration = as.numeric(sapply(slogan, detect_alliteration)),
          has_personification = as.numeric(sapply(slogan, detect_personification)),

          # Semantic features
          has_sensory = str_detect(tolower(slogan), linguistic_patterns$sensory_words),
          has_metaphor = str_detect(tolower(slogan), linguistic_patterns$metaphorical),
          has_simile = str_detect(tolower(slogan), linguistic_patterns$simile),
          has_hyperbole = str_detect(tolower(slogan), linguistic_patterns$hyperbolic),
          has_local_lang = as.numeric(sapply(slogan, detect_local_language)),
          has_indigenous = str_detect(tolower(slogan), linguistic_patterns$indigenous_terms),

          # Pronoun usage
          uses_inclusive = str_detect(tolower(slogan), linguistic_patterns$inclusive_we),
          uses_direct = str_detect(tolower(slogan), linguistic_patterns$direct_you),
          uses_submissive = str_detect(tolower(slogan), linguistic_patterns$submissive_her)
        )

      feature_summary <- linguistic_data %>%
        group_by(cluster) %>%
        summarise(
          # Basic stats
          Avg_Word_Count = round(mean(wc), 2),
          Avg_Char_Count = round(mean(char_count), 2),
          Avg_Word_Length = round(mean(avg_word_length), 2),
          Avg_CWR = round(mean(cwr, na.rm = TRUE), 2),

          # Structural features (%)
          Pct_Imperative = round(mean(has_imperative) * 100, 1),
          Pct_Interrogative = round(mean(has_interrogative) * 100, 1),
          Pct_Rhyme = round(mean(has_rhyme) * 100, 1),
          Pct_Alliteration = round(mean(has_alliteration) * 100, 1),

          # Semantic features (%)
          Pct_Sensory = round(mean(has_sensory) * 100, 1),
          Pct_Metaphor = round(mean(has_metaphor) * 100, 1),
          Pct_Simile = round(mean(has_simile) * 100, 1),
          Pct_Hyperbole = round(mean(has_hyperbole) * 100, 1),
          Pct_Local_Lang = round(mean(has_local_lang) * 100, 1),
          Pct_Indigenous = round(mean(has_indigenous) * 100, 1),

          # Pronoun usage (%)
          Pct_Inclusive = round(mean(uses_inclusive) * 100, 1),
          Pct_Direct = round(mean(uses_direct) * 100, 1),
          Pct_Submissive = round(mean(uses_submissive) * 100, 1),
          .groups = "drop"
        )

      write.csv(feature_summary, file, row.names = FALSE)
    }
  )

  output$download_dissimilarity <- downloadHandler(
    filename = function() {
      paste0("slogan_dissimilarity_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(values$data)
      data <- values$data
      data$distance <- NA

      # For each cluster, compute within-cluster sum of dissimilarities
      for (cl in unique(data$cluster)) {
        cluster_rows <- which(data$cluster == cl &
          !is.na(data$valence) &
          !is.na(data$arousal) &
          !is.na(data$dominance))
        if (length(cluster_rows) > 1) {
          cluster_data <- data[cluster_rows, c("valence", "arousal", "dominance")]
          dist_matrix <- as.matrix(daisy(cluster_data))
          data$distance[cluster_rows] <- rowSums(dist_matrix, na.rm = TRUE)
        } else if (length(cluster_rows) == 1) {
          data$distance[cluster_rows] <- 0
        }
      }

      write.csv(data, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$analysis_summary <- renderText({
    req(values$data)

    tryCatch(
      {
        safe_range <- function(x, col_name) {
          numeric_vals <- as.numeric(x)
          numeric_vals <- numeric_vals[!is.na(numeric_vals)]
          if (length(numeric_vals) == 0) {
            return(paste(col_name, "range: No valid data"))
          }
          return(paste(
            col_name, "range:", round(min(numeric_vals), 3),
            "-", round(max(numeric_vals), 3)
          ))
        }

        paste(
          "Analysis Summary:\n",
          "- Total countries analyzed:", nrow(values$data), "\n",
          "- Number of clusters:", length(unique(values$data$cluster)), "\n",
          "- Avg total words:", sprintf("%.2f", mean(values$data$wc, na.rm = TRUE)), "\n",
          "- Avg content words:", sprintf("%.2f", mean(values$data$cw, na.rm = TRUE)), "\n",
          "- Avg CWR:", sprintf("%.2f", mean(values$data$cwr, na.rm = TRUE)), "\n",
          "-", safe_range(values$data$valence, "Valence"), "\n",
          "-", safe_range(values$data$arousal, "Arousal"), "\n",
          "-", safe_range(values$data$dominance, "Dominance")
        )
      },
      error = function(e) {
        paste(
          "Analysis Summary:\n",
          "Error generating summary:",
          e$message
        )
      }
    )
  })
} # <-- Add this to close the server function

# =============================================================================
# RUN THE APP
# =============================================================================

shinyApp(ui = ui, server = server)

# (Optionally add a blank line here)
