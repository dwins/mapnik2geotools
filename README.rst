what's this?
------------

A small scala project with the goal of transforming
`Mapnik <http://mapnik.org/>`_ map definitions to
something `GeoTools <http://geotools.org/>`_ and
`GeoServer <http://geoserver.org/>`_ can deal with.

setup
-----

1. Install SBT. See the instructions at http://simple-build-tool.googlecode.com/

2. ``sbt update`` to fetch dependencies
 
3. ``sbt compile`` to build everything
 
4. ``sbt "run my_mapnik_file.xml"`` to run it (see options below)

options (overview)
------------------

sbt "run [options] {mapnik xml file}"

* -- rest= (url to geoserver rest api, like
  http://localhost:8080/geoserver/rest).  This tells mapnik2geotools to make
  REST calls to GeoServer to duplicate a given mapnik.xml file.  Currently this means:

    * Upload one style for every <Style> element referenced in any <Layer>

    * Configure raster/vector layers for the following DataSource types:

      * geotiff

      * shapefile

      * postgis

    * Configure a GeoServer LayerGroup so that you can conveniently reference
      them all in a single WMS request

  You also need to provide:
    * --datadir=(some path). The tool creates SLD image references based on this parameter.

  Additional options which affect the configuration set via REST include:
    
    * --user=(username) [defaults to ``admin``]. This is the username that the
      tool will use to authenticate itself with GeoServer.
    * --password=(password) [defaults to ``geoserver``].  This is the password
      that the tool will use to authenticate itself with GeoServer.
    * --prefix=(some xml prefix) [defaults to ``mn2gt``].  This is the prefix
      of a workspace that will be associated with all layers uploaded (except
      the containing layergroup since those do not have workspaces).  You must
      create the workspace yourself before running the tool.
    * --namespace=(some xml namespace URI) [defaults to ``http://mn2gt.com/``].
      This is the URI for the workspace mentioned above.

If you don't provide a REST URL, the tool instead dumps some output to the filesystem:
   
  * One SLD file per <Style> element in the Mapnik XML
  * One (database).sql file for each database referenced in the Mapnik XML.
    These contain ``CREATE VIEW`` statements used to workaround the fact that
    mapnik allows prefiltering data by using a SQL subquery in place of a table
    name.  mapnik2geotools assumes you've already run these sql scripts before
    you run the REST configuration.

usage
-----

There's currently no standalone launcher script, so the recommended usage is to
start up the SBT prompt by running ``sbt`` with no arguments.  You can then use
the ``run`` task to execute the converter.  Currently, you have to run it twice
to get GeoServer fully setup:

1. First, generate files on disk by running with the name of a mapnik map
   definition::

      run /path/to/mapnik/map.xml

   This creates one SLD file in ``/path/to/mapnik/output/`` for each ``Style``
   element in the mapfile.  You can inspect these manually (when uploading to
   GeoServer through REST the styles will be rewritten.)  There is also a
   ``{dbname}.sql`` file ({dbname} will be substituted for the name of the
   database) which creates views for each datastore that uses a subquery.  If
   you use Postgis, you should execute this SQL against your database to set up
   those aliases for GeoServer (as GeoServer does not currently support
   subqueries in datastore definitions).  This looks like::

      psql -d mydb -f /path/to/mapnik/output/mydb.sql

2. With the tables and views in place, run the converter with a GeoServer REST
   URL and datadir::

      run /path/to/mapnik/map.xml --rest http://localhost:8080/geoserver/rest --datadir /path/to/geoserver_data
   Optionally, you can also provide --user <geoserver user name> and --password
   <geoserver user password>.  If you omit these, the default admin:geoserver
   credentials will be assumed.

bugs/feature requests
---------------------
many. let me know what you find :)

mailing list
------------

There is a public mailing list at
https://groups.google.com/a/opengeo.org/group/mapnik2geotools/topics where you
can ask for help and discuss new feature ideas.
