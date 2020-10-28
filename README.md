
<!-- README.md is generated from README.Rmd. Please edit that file -->

## HOQCwfs

<!-- badges: start -->

<!-- badges: end -->

### R Web Feature Service (WFS) read-only interface

This package contains functions to retrieve information from a Web
Feature Service (WFS) resource. It generates HTML requests for the WFS
API.

### Installation

You can install this version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("HanOostdijk/HOQCwfs")
```

### Example

A typical workflow looks like this :

#### Initialize

Load the package and indicate which WFS resource you want to access and
the version that you want to use for the requests and their answers. In
this way you can avoid specifying this in the separate function calls.

``` r
library(HOQCwfs)
WFS_set_url()      # default https://geoweb.amstelveen.nl/geoserver/topp/wfs will be used
WFS_set_version()  # default 1.1.0 will be used
```

#### Look at the possibilities the WFS resource has to offer

Retrieve the GetCapabilities document that describes the various
features of the WFS resource and the actions that can be done. Because I
have only read-access to WFS resources I am most interested in the query
actions. An ‘impression’ of the rather extensive GetCapabilities xml
document is given below.

``` r
xmlcap <- WFS_getcapabilities()  
class(xmlcap)
#> [1] "xml_document" "xml_node"
```

    #>  {xml_document}
    #>  <WFS_Capabilities version="1.1.0" schemaLocation="http://www.opengis.net/wfs https://geoweb.amstelveen.nl/geo
    #> server/schemas/wfs/1.1.0/wfs.xsd" updateSequence="5217" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
    #> xmlns:wfs="http://www.opengis.net/wfs" xmlns:ows="http://www.opengis.net/ows" xmlns:gml="http://www.opengis.ne
    #> t/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:topp="http://ww
    #> w.openplans.org/topp">
    #>  [1] <ows:ServiceIdentification>\n  <ows:Title>GeoServer Web Feature Service</ ...
    #>  [2] <ows:ServiceProvider>\n  <ows:ProviderName>gemeente Amstelveen</ows:Provi ...
    #>  [3] <ows:OperationsMetadata>\n  <ows:Operation name="GetCapabilities">\n    < ...
    #>  [4] <FeatureTypeList>\n  <Operations>\n    <Operation>Query</Operation>\n     ...
    #>  [5] <ogc:Filter_Capabilities>\n  <ogc:Spatial_Capabilities>\n    <ogc:Geometr ...

The xml document can be written to a local dataset with the `out_path`
argument:

``` r
xmlcap <- WFS_getcapabilities(out_path='mycap.xml') 
```

#### Look at the various features of the WFS resource

From the GetCapabilities document we can extract the features that are
available:

``` r
ft1 <- WFS_featuretypes(xmlcap)
dim(ft1)
#> [1] 206   4
```

When there are many features (206 in this case) and we are just
interested in feature names that contain the characters ‘bomen’
(i.e. ‘trees’) we can filter the names in the following way that
results in only 8 cases

``` r
ft2 <- WFS_featuretypes(xmlcap,
       filternames=stringr::fixed("bomen", ignore_case = T))
```

``` r
class(ft2)
#> [1] "tbl_df"     "tbl"        "data.frame"
dim(ft2)
#> [1] 8 4
ft2$layer
#> [1] "topp:bgt_bomen"                    "topp:bomen_obsurv"                
#> [3] "topp:gidw_groenbomen"              "topp:groenbeheer_amr_bomen_p"     
#> [5] "topp:groenbeheer_asv_bomen_koppel" "topp:niet_vervallen_wv_bomen_av"  
#> [7] "topp:waardevolle_bomen_aalsmeer"   "topp:wv_bomen_aalsmeer"
str(head(ft2,2)) # show only first two 'bomen' featuretypes 
#> tibble [2 x 4] (S3: tbl_df/tbl/data.frame)
#>  $ layer     : chr [1:2] "topp:bgt_bomen" "topp:bomen_obsurv"
#>  $ defaultcrs: chr [1:2] "urn:x-ogc:def:crs:EPSG:28992" "urn:x-ogc:def:crs:EPSG:28992"
#>  $ lc_wgs84  : chr [1:2] "4.7235949130419375 52.217343780971795" "4.716107462196754 52.219588044310704"
#>  $ uc_wgs84  : chr [1:2] "4.910540424473338 52.32262507211209" "4.823804208755314 52.296765954101396"
```

#### Look at the data fields of featuretypes

For this we use the function `WFS_describefeaturetype` that executes the
WFS `DescribeFeatureType` request. As an example lets us look at the
first and third featuretype in ft2 (`topp:bgt_bomen` and
`topp:gidw_groenbomen`). Here we only show the first `5` rows of the
result tibble of 14 rows.

``` r
bomen_fields_1_3<- WFS_describefeaturetype(ft2$layer[c(1,3)])
```

``` r
head(bomen_fields_1_3,5)
#> # A tibble: 5 x 2
#>   typename             name          
#>   <chr>                <chr>         
#> 1 topp:bgt_bomen       id            
#> 2 topp:bgt_bomen       geometrie     
#> 3 topp:gidw_groenbomen aantal        
#> 4 topp:gidw_groenbomen boom_omschrijf
#> 5 topp:gidw_groenbomen groep
```

Because we later will use the fields of `topp:gidw_groenbomen` we will
show all fields for this featuretype. Because we now request only one
featuretype we can save the result of `DescribeFeatureType` in an xml
file. Then we have the standard output:

``` r
WFS_describefeaturetype("topp:gidw_groenbomen",out_path = 'desc.xml')
#> # A tibble: 12 x 2
#>    typename             name           
#>    <chr>                <chr>          
#>  1 topp:gidw_groenbomen aantal         
#>  2 topp:gidw_groenbomen boom_omschrijf 
#>  3 topp:gidw_groenbomen groep          
#>  4 topp:gidw_groenbomen groep_omschrijf
#>  5 topp:gidw_groenbomen jaar           
#>  6 topp:gidw_groenbomen mslink         
#>  7 topp:gidw_groenbomen mutdatum       
#>  8 topp:gidw_groenbomen objec_omschrijf
#>  9 topp:gidw_groenbomen uitv_omschrijf 
#> 10 topp:gidw_groenbomen vveld2         
#> 11 topp:gidw_groenbomen objec          
#> 12 topp:gidw_groenbomen geometrie
```

and the `desc.xml` file with its contents:

    #>  <?xml version="1.0" encoding="UTF-8"?>
    #>  <xsd:schema xmlns:gml="http://www.opengis.net/gml" xmlns:topp="http://www.openplans.org/topp" xmlns:xsd="http
    #> ://www.w3.org/2001/XMLSchema" elementFormDefault="qualified" targetNamespace="http://www.openplans.org/topp">
    #>    <xsd:import namespace="http://www.opengis.net/gml" schemaLocation="https://geoweb.amstelveen.nl/geoserver/s
    #> chemas/gml/3.1.1/base/gml.xsd"/>
    #>    <xsd:complexType name="gidw_groenbomenType">
    #>      <xsd:complexContent>
    #>        <xsd:extension base="gml:AbstractFeatureType">
    #>          <xsd:sequence>
    #>            <xsd:element maxOccurs="1" minOccurs="0" name="aantal" nillable="true" type="xsd:string"/>
    #>            <xsd:element maxOccurs="1" minOccurs="0" name="boom_omschrijf" nillable="true" type="xsd:string"/>
    #>            <xsd:element maxOccurs="1" minOccurs="0" name="groep" nillable="true" type="xsd:string"/>
    #>            <xsd:element maxOccurs="1" minOccurs="0" name="groep_omschrijf" nillable="true" type="xsd:string"/>
    #>            <xsd:element maxOccurs="1" minOccurs="0" name="jaar" nillable="true" type="xsd:string"/>
    #>            <xsd:element maxOccurs="1" minOccurs="0" name="mslink" nillable="true" type="xsd:string"/>
    #>            <xsd:element maxOccurs="1" minOccurs="0" name="mutdatum" nillable="true" type="xsd:string"/>
    #>            <xsd:element maxOccurs="1" minOccurs="0" name="objec_omschrijf" nillable="true" type="xsd:string"/>
    #>            <xsd:element maxOccurs="1" minOccurs="0" name="uitv_omschrijf" nillable="true" type="xsd:string"/>
    #>            <xsd:element maxOccurs="1" minOccurs="0" name="vveld2" nillable="true" type="xsd:string"/>
    #>            <xsd:element maxOccurs="1" minOccurs="0" name="objec" nillable="true" type="xsd:string"/>
    #>            <xsd:element maxOccurs="1" minOccurs="1" name="geometrie" nillable="false" type="gml:MultiPointProp
    #> ertyType"/>
    #>          </xsd:sequence>
    #>        </xsd:extension>
    #>      </xsd:complexContent>
    #>    </xsd:complexType>
    #>    <xsd:element name="gidw_groenbomen" substitutionGroup="gml:_Feature" type="topp:gidw_groenbomenType"/>
    #>  </xsd:schema>

#### Retrieve the actual data

Now that we know the metadata of our resource we can actually retrieve
the attributes and geometrical data of the features. We do this with
`WFS_getfeature('typename')` to:  
\- retrieve all features with all attributes and the geometry: The `id`
attribute will also be returned - retrieve some features and the
geometry: add `cql_filter=` or `filter=` with a filter expression -
retrieve all features for some attributes and the geometry: add
`propertyname='a1,a2'` to select the attributes `a1` and `a2`. The `id`
attribute and the `geometry` will also be returned - retrieve a subset
of one of the above: add `maxfeatures=` (1.1.0) or `count=` (2.0.0)
optionally in combination with `startindex=`

If we are only interested in the number of features that would be
returned we can add `resultType=hits` to any of the above. This only
returns this number without any other data.

Examples:

##### retrieve all features with all attributes and the geometry

``` r
typename  <- 'topp:gidw_groenbomen'
wfs1      <-  WFS_getfeature(typename)
class(wfs1)
#> [1] "sf"         "data.frame"
print(wfs1,n=5) # show only first 5 features
#> Simple feature collection with 43202 features and 12 fields
#> geometry type:  MULTIPOINT
#> dimension:      XY
#> bbox:           xmin: 114548 ymin: 472822.5 xmax: 122353.6 ymax: 481726.2
#> projected CRS:  Amersfoort / RD New
#> First 5 features:
#>                  id aantal                     boom_omschrijf groep
#> 1 gidw_groenbomen.1      1         Acer platanoides 'Deborah'    BV
#> 2 gidw_groenbomen.2      1         Acer platanoides 'Deborah'    BV
#> 3 gidw_groenbomen.3      1                 Platanus hispanica    BV
#> 4 gidw_groenbomen.4      1                 Platanus hispanica    BV
#> 5 gidw_groenbomen.5      1 Populus canad. 'Serotina de Selys'    BP
#>           groep_omschrijf jaar mslink mutdatum             objec_omschrijf
#> 1      Boom in verharding 1955  20645 20170111             Keizer Karelweg
#> 2      Boom in verharding 1955  20646 20170111             Keizer Karelweg
#> 3      Boom in verharding 1955  20636 20170111              Amsterdamseweg
#> 4      Boom in verharding 1955  20637 20170111              Amsterdamseweg
#> 5 Boom in beplanting/berm 1955  20638 20161214 Keizer Karelweg/Bella Donna
#>                        uitv_omschrijf vveld2 objec
#> 1 Bestek 1 - Randwijck/Elsrijk Oost23    120 20608
#> 2 Bestek 1 - Randwijck/Elsrijk Oost23    117 20608
#> 3 Bestek 1 - Randwijck/Elsrijk Oost23    115 20602
#> 4 Bestek 1 - Randwijck/Elsrijk Oost23    114 20602
#> 5 Bestek 1 - Randwijck/Elsrijk Oost23    106 20607
#>                         geometry
#> 1 MULTIPOINT ((118817.8 48078...
#> 2 MULTIPOINT ((118818.5 48080...
#> 3 MULTIPOINT ((118744.7 48096...
#> 4 MULTIPOINT ((118745.1 48097...
#> 5 MULTIPOINT ((118754.6 48075...
```

##### retrieve some features

For non-complex queries the `cql_filter` can be used. The features for
the tree `Prunus serrulata 'Kanzan'` can be queried in the following
way. The numbers of quotes in a query is often confusing (for me at
least): here a set of quotes is needed because `boom_omschrijf` is a
character field and quotes around ‘Kanzan’ have to be duplicated.

``` r
tree_name <- "Prunus serrulata ''Kanzan''"  
tree_exp  <- glue::glue("boom_omschrijf='{tree_name}'")
print(tree_exp)
#> boom_omschrijf='Prunus serrulata ''Kanzan'''
wfs2      <-  WFS_getfeature(typename,cql_filter=tree_exp )
print(wfs2,n=5,digits=5) # show only first 5 features, 5 digits in geometry
#> Simple feature collection with 210 features and 12 fields
#> geometry type:  MULTIPOINT
#> dimension:      XY
#> bbox:           xmin: 116547.4 ymin: 477231.5 xmax: 120393.8 ymax: 480755.5
#> projected CRS:  Amersfoort / RD New
#> First 5 features:
#>                     id aantal            boom_omschrijf groep
#> 1 gidw_groenbomen.2055      1 Prunus serrulata 'Kanzan'    BV
#> 2 gidw_groenbomen.2086      1 Prunus serrulata 'Kanzan'    BP
#> 3 gidw_groenbomen.2126      1 Prunus serrulata 'Kanzan'    BP
#> 4 gidw_groenbomen.2127      1 Prunus serrulata 'Kanzan'    BP
#> 5 gidw_groenbomen.2128      1 Prunus serrulata 'Kanzan'    BP
#>           groep_omschrijf jaar mslink mutdatum
#> 1      Boom in verharding 2011  31772 20180115
#> 2 Boom in beplanting/berm 1960  31044 20170111
#> 3 Boom in beplanting/berm 1998  26916 20170111
#> 4 Boom in beplanting/berm 1998  26917 20170111
#> 5 Boom in beplanting/berm 1998  26918 20170111
#>                          objec_omschrijf
#> 1                        Wibautlaan 2-24
#> 2 G. van Prinstererlaan 55a. Palet Noord
#> 3                     Heemraadschapslaan
#> 4                     Heemraadschapslaan
#> 5                     Heemraadschapslaan
#>                              uitv_omschrijf vveld2 objec
#> 1 Patrimonium/Randwijck/Elsrijk - Kees Muis    101 22113
#> 2 Patrimonium/Randwijck/Elsrijk - Kees Muis    102 21810
#> 3 Patrimonium/Randwijck/Elsrijk - Kees Muis    114 21703
#> 4 Patrimonium/Randwijck/Elsrijk - Kees Muis    113 21703
#> 5 Patrimonium/Randwijck/Elsrijk - Kees Muis    112 21703
#>                       geometry
#> 1 MULTIPOINT ((118655 479642))
#> 2 MULTIPOINT ((119048 479984))
#> 3 MULTIPOINT ((118580 480068))
#> 4 MULTIPOINT ((118569 480069))
#> 5 MULTIPOINT ((118556 480069))
```

##### retrieve some attributes

When we are only interested in the ‘omschrijvingen’ (descriptions)
fields we can add the `propertyname` argument:

``` r
tree_name <- "Prunus serrulata ''Kanzan''"  
tree_exp  <- glue::glue("boom_omschrijf='{tree_name}'")
fields    <- 'boom_omschrijf,objec_omschrijf'
wfs3      <-  WFS_getfeature(typename,cql_filter=tree_exp,propertyname=fields )
print(wfs3,n=5,digits=5) # show only first 5 features, 5 digits in geometry
#> Simple feature collection with 210 features and 3 fields
#> geometry type:  MULTIPOINT
#> dimension:      XY
#> bbox:           xmin: 116547.4 ymin: 477231.5 xmax: 120393.8 ymax: 480755.5
#> projected CRS:  Amersfoort / RD New
#> First 5 features:
#>                     id            boom_omschrijf
#> 1 gidw_groenbomen.2055 Prunus serrulata 'Kanzan'
#> 2 gidw_groenbomen.2086 Prunus serrulata 'Kanzan'
#> 3 gidw_groenbomen.2126 Prunus serrulata 'Kanzan'
#> 4 gidw_groenbomen.2127 Prunus serrulata 'Kanzan'
#> 5 gidw_groenbomen.2128 Prunus serrulata 'Kanzan'
#>                          objec_omschrijf                     geometry
#> 1                        Wibautlaan 2-24 MULTIPOINT ((118655 479642))
#> 2 G. van Prinstererlaan 55a. Palet Noord MULTIPOINT ((119048 479984))
#> 3                     Heemraadschapslaan MULTIPOINT ((118580 480068))
#> 4                     Heemraadschapslaan MULTIPOINT ((118569 480069))
#> 5                     Heemraadschapslaan MULTIPOINT ((118556 480069))
```

##### retrieve the number of results

If we want to know beforehand the number of features that a query would
return we can add the `resultType=hits` argument:

``` r
  WFS_getfeature(typename,cql_filter=tree_exp,propertyname=fields,resultType='hits' )
#> [1] 210
```

##### change the Spatial Reference System

In the output of the previous examples we saw that the coordinates in
the geometry were denoted in the Coordinate Reference System (CRS) that
is used in the Netherlands: ‘Amersfoort / RD New’ also known as
‘EPSG:28992’. If we want to have these coordinates denoted in the more
commonly used ‘WGS 84’ (EPSG:4326) in terms of longitude and latitude we
can add the `srsName` argument. SRS ([Spatial Reference
System](https://en.wikipedia.org/wiki/Spatial_reference_system)) is a
synonym for CRS.

``` r
wfs4      <-  WFS_getfeature(typename,cql_filter=tree_exp,propertyname=fields,srsName='EPSG:4326' )
print(wfs4,n=5,digits=5) # show only first 5 features, 5 digits in geometry
#> Simple feature collection with 210 features and 3 fields
#> geometry type:  MULTIPOINT
#> dimension:      XY
#> bbox:           xmin: 4.823706 ymin: 52.28175 xmax: 4.879813 ymax: 52.31365
#> geographic CRS: WGS 84
#> First 5 features:
#>                     id            boom_omschrijf
#> 1 gidw_groenbomen.2055 Prunus serrulata 'Kanzan'
#> 2 gidw_groenbomen.2086 Prunus serrulata 'Kanzan'
#> 3 gidw_groenbomen.2126 Prunus serrulata 'Kanzan'
#> 4 gidw_groenbomen.2127 Prunus serrulata 'Kanzan'
#> 5 gidw_groenbomen.2128 Prunus serrulata 'Kanzan'
#>                          objec_omschrijf                     geometry
#> 1                        Wibautlaan 2-24 MULTIPOINT ((4.8543 52.304))
#> 2 G. van Prinstererlaan 55a. Palet Noord MULTIPOINT ((4.8601 52.307))
#> 3                     Heemraadschapslaan MULTIPOINT ((4.8532 52.307))
#> 4                     Heemraadschapslaan  MULTIPOINT ((4.853 52.307))
#> 5                     Heemraadschapslaan MULTIPOINT ((4.8528 52.307))
```

##### retrieve a subset of the query results with `startindex`, `maxfeatures` and `count` and using `verbose`

With the `startindex` and `maxfeatures` (1.1.0) or `count` (2.0.0)
argument a subset of a query is produced. The `WFS__getfeature`
functions changes the argument when necessary. By added the `verbose=T`
clause the generated request url will be displayed. Notice that
`startIndex=2` indicates that the first feature to be retrieved is
number 3. In other words `startIndex` indicates the number of records to
skip.

``` r
wfs5a     <-  WFS_getfeature(typename,cql_filter=tree_exp,startIndex=2,maxfeatures=6,
                             propertyname=fields,srsName='EPSG:4326'
                             ,verbose=T,version='1.1.0' )
```

    #>  https://geoweb.amstelveen.nl/geoserver/topp/wfs?service=WFS&version=1.1.0&request=GetFeature
    #> &typename=topp:gidw_groenbomen&outputFormat=application/json&cql_filter=boom_omschrijf='Prunu
    #> s serrulata ''Kanzan'''&startIndex=2&maxfeatures=6&propertyname=boom_omschrijf,objec_omschrij
    #> f&srsName=EPSG:4326
    #>  Success: (200) OK

``` r
print(wfs5a,n=5,digits=5) # show only first 5 features, 5 digits in geometry
#> Simple feature collection with 6 features and 3 fields
#> geometry type:  MULTIPOINT
#> dimension:      XY
#> bbox:           xmin: 4.852843 ymin: 52.30288 xmax: 4.859176 ymax: 52.30738
#> geographic CRS: WGS 84
#> First 5 features:
#>                     id            boom_omschrijf    objec_omschrijf
#> 1 gidw_groenbomen.2126 Prunus serrulata 'Kanzan' Heemraadschapslaan
#> 2 gidw_groenbomen.2127 Prunus serrulata 'Kanzan' Heemraadschapslaan
#> 3 gidw_groenbomen.2128 Prunus serrulata 'Kanzan' Heemraadschapslaan
#> 4 gidw_groenbomen.2135 Prunus serrulata 'Kanzan'    Wibautlaan 2-24
#> 5 gidw_groenbomen.2136 Prunus serrulata 'Kanzan'    Wibautlaan 2-24
#>                       geometry
#> 1 MULTIPOINT ((4.8532 52.307))
#> 2  MULTIPOINT ((4.853 52.307))
#> 3 MULTIPOINT ((4.8528 52.307))
#> 4  MULTIPOINT ((4.854 52.303))
#> 5  MULTIPOINT ((4.854 52.303))
```

``` r
wfs5b     <-  WFS_getfeature(typename,cql_filter=tree_exp,startIndex=2,maxfeatures=6,
                             propertyname=fields,srsName='EPSG:4326'
                             ,verbose=T,version='2.0.0' )
```

    #>  https://geoweb.amstelveen.nl/geoserver/topp/wfs?service=WFS&version=2.0.0&request=GetFeature
    #> &typename=topp:gidw_groenbomen&outputFormat=application/json&cql_filter=boom_omschrijf='Prunu
    #> s serrulata ''Kanzan'''&startIndex=2&count=6&propertyname=boom_omschrijf,objec_omschrijf&srsN
    #> ame=EPSG:4326
    #>  Success: (200) OK

``` r
identical(wfs5a,wfs5b)
#> [1] TRUE
```
