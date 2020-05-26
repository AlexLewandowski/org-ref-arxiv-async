# org-ref-arxiv-async
This slimmed down version of org-ref uses only the bibtex formatting and arxiv fetching features. Perfect for users of ivy-bibtex that would like only a few of org-ref's features.

# Relevant autoloads and changes

## clean-and-sort-bibtex-entry
Combines some of org-ref's formatting hooks into a single function since hook processing wasn't working.

## bibtex-set-field 
FNo changes, moved from org-ref-core. unction to set bibtex field with some value.

## bibtex-set-keywords
No changes, moved from org-ref-core. This is basically bibtex-set-field for keywords. Treated a bit differently because it functions as a list.

## bibtex-keywords
No changes, moved from org-ref-core. Fetches the keywords for a bibtex entry at point.

## arxiv-get-pdf-add-bibtex-entry-async
A few changes have been made so that it can parse the entire link and extract only the relevant arxiv ID.

## arxiv-add-bibtex-entry-async
Adapted from arxiv-add-bibtex-entry, uses clean-and-sort-bibtex-entry.

## arxiv-get-pdf-async
Fetches the pdf associated with the arxiv ID asynchronously. 
