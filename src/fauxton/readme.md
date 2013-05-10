Fauxton
=======

This is the initial implementation of Fauxton, focused on fleshing out
the various pieces of functionality and as a test bed for new ideas.
Full functionality and design considerations will be added later.



Current items of interest:

  * Live JSON editor with dynamic JS Hinting and error popups
  * Initial plugin system
  * Minimal externally loadable plugin example
  * Data popups for additional db info on \_all_dbs page
  * CouchDB API compliant urls



### CouchDB Setup ###

    Clone the Couchdb repo: https://github.com/apache/couchdb.git or http://git-wip-us.apache.org/repos/asf/couchdb.git

    The easiest way to use fauxton, especially when developing for it, is to link it to your existing couch _utils directory. Eg:
    `ln -s couchdb/src/fauxton /Applications/Apache\ CouchDB.app/Contents/Resources/couchdbx-core/share/couchdb/www`

    It will be available at [http://localhost:5984/_utils/fauxton/](http://localhost:5984/_utils/fauxton/)

### Development

    cd couchdb/src/fauxton

    To set fauxton into develop mode, run `./rebuild`
    For production, after code changes, run `./rebuild compile` (requires [jamjs](http://jamjs.org/docs))


### As a couchapp

     Install [erica](https://github.com/benoitc/erica)

     To deploy to your local [Couchdb instance] (http://localhost:5984/fauxton/_design/fauxton/_rewrite/)
    `./push http://admin:pass@localhost:5984/fauxton'

    For production, after code changes (requires [jamjs](http://jamjs.org/docs)) run
    `./push http://admin:pass@localhost:5984/fauxton compile`

## Understang Fauxton Code layout

Each bit of functionality is its own seperate module or addon. All core modules are stored under `js/module` and any addons that are optional are under `js/addons`.
We use [backbone.js](http://backbonejs.org/) and [Backbone.layoutmanager](https://github.com/tbranyen/backbone.layoutmanager) quite heavily, so best to get an idea how they work.
Its best at this point to read through a couple of the modules and addons to get an idea of how they work. Two good starting points are `js/addon/config` and `js/modules/databases`.
Each module must have a `base.js` file, this is read and compile when Fauxton is deployed. A `resource.js` file is usually for your Backbone.Models and Backbone.Collections,
`view.js` for your Backbone.Views. The `routes.js` is used to register a url path for your view along with what layout, data, breadcrumbs and api point is required for the view.

## Todo items

Checkout [Jira](https://issues.apache.org/jira/browse/COUCHDB/component/12320406) for a list of items to do.

