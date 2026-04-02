# sparqlr

A SPARQL client for R.

`sparqlr` is an R library that allows running SPARQL queries directly from R.
SPARQL (short for SPARQL Protocol and RDF Query Language) is a query language
used to retrieve and manipulate data stored in RDF (Resource Description
Framework) format.

<br>

## Installation

### Installing from CRAN

The latest stable version of the package can be installed from CRAN.
Start an R session, then run:

```R
install.packages("sparqlr")
```

### Installing from GitHub

The latest development (unstable) version of the package can be found on
GitHub. Start an R session, then run:

```R
remotes::install_github("sib-swiss/sparqlr")
```

> *Note:* running the above command requires you to have the `remotes` R
> package installed on your machine.  
> Should that not already be the case, you can install it with the command:
> `install.packages("remotes")`.

<br>

## Usage examples

Running a simple SELECT query:

```R
query <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?message
WHERE{
    wd:Q131303 rdfs:label ?message
    FILTER( LANG( ?message ) = 'en' )
}
"

sparql_select(
  endpoint = "https://query.wikidata.org/sparql",
  query = query 
)
```

Running a simple CONSTRUCT query:

```R
query <- "CONSTRUCT {
      wd:Q166794  wdt:P2974  ?habitat  .
}
WHERE {
    wd:Q166794  ?p         ?o        ;
                wdt:P2974  ?habitat  .
}
"

sparql_construct(
  endpoint = "https://query.wikidata.org/sparql",
  query = query 
)
```

Running a simple DESCRIBE query:

```R
query <- "DESCRIBE wd:Q166794
LIMIT 50
"

sparql_describe(
  endpoint = "https://query.wikidata.org/sparql",
  query = query 
)
```

<br>
