
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
    #> server/schemas/wfs/1.1.0/wfs.xsd" updateSequence="4803" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
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
#> [1] 201   4
```

When there are many features (201 in this case) and we are just
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
