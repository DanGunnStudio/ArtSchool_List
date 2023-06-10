---
editor_options: 
  markdown: 
    wrap: 72
---

README

# Art School Scrape

About: Uses polite package to scrape the NASAD website for a list of
accredited art degree granting colleges and universities in the US.

## Data Dictionary - data/supercleaned_artschool_df.csv

| Col1        | Col2                                                   | Col3        |
|-------------|--------------------------------------------------------|-------------|
| id          | index number                                           | (numeric)   |
| school_name | name of the university                                 | (character) |
| street      | street address for the university                      | (character) |
| url         | web address of art program                             | (character) |
| degree_type | full list of the degree types and subjects unseparated | (character) |
| about       | about the university                                   | (character) |
| zip         | zip code / US postal code                              | (character) |
| state       | state abbreviation                                     | (character) |
| degree.1    | degree type                                            | (character) |
| years       | length of course of study                              | (character) |
| subject     | subject or major course of study                       | (character) |
| school_type | school type, ie. public / private etc.                 | (character) |

\
Used to build the NASAD Art Schools Dashboard: [Tableau
Dashboard](https://public.tableau.com/views/ArtSchoolList/ArtSchoolListDashboard?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link)

[![](images/Art%20School%20List%20Dashboard.png)](https://public.tableau.com/app/profile/dan.gunn/viz/ArtSchoolList/ArtSchoolListDashboard?publish=yes)

### Contributing Guidelines:

I welcome contributions from the community.
