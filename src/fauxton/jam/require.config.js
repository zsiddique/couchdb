var jam = {
    "packages": [
        {
            "name": "backbone",
            "location": "jam/backbone",
            "main": "backbone.js"
        },
        {
            "name": "backbone.layoutmanager",
            "location": "jam/backbone.layoutmanager",
            "main": "backbone.layoutmanager.js"
        },
        {
            "name": "bootstrap",
            "location": "jam/bootstrap"
        },
        {
            "name": "codemirror",
            "location": "jam/codemirror",
            "main": "lib/codemirror.js"
        },
        {
            "name": "d3",
            "location": "jam/d3",
            "main": "d3.js"
        },
        {
            "name": "jquery",
            "location": "jam/jquery",
            "main": "dist/jquery.js"
        },
        {
            "name": "lessc",
            "location": "jam/lessc",
            "main": "lessc.js"
        },
        {
            "name": "lodash",
            "location": "jam/lodash",
            "main": "./dist/lodash.underscore.js"
        },
        {
            "name": "nvd3",
            "location": "jam/nvd3"
        },
        {
            "name": "text",
            "location": "jam/text",
            "main": "text.js"
        },
        {
            "name": "underscore",
            "location": "jam/underscore",
            "main": "underscore.js"
        }
    ],
    "version": "0.2.17",
    "shim": {
        "d3": {
            "exports": "d3"
        }
    }
};

if (typeof require !== "undefined" && require.config) {
    require.config({
    "packages": [
        {
            "name": "backbone",
            "location": "jam/backbone",
            "main": "backbone.js"
        },
        {
            "name": "backbone.layoutmanager",
            "location": "jam/backbone.layoutmanager",
            "main": "backbone.layoutmanager.js"
        },
        {
            "name": "bootstrap",
            "location": "jam/bootstrap"
        },
        {
            "name": "codemirror",
            "location": "jam/codemirror",
            "main": "lib/codemirror.js"
        },
        {
            "name": "d3",
            "location": "jam/d3",
            "main": "d3.js"
        },
        {
            "name": "jquery",
            "location": "jam/jquery",
            "main": "dist/jquery.js"
        },
        {
            "name": "lessc",
            "location": "jam/lessc",
            "main": "lessc.js"
        },
        {
            "name": "lodash",
            "location": "jam/lodash",
            "main": "./dist/lodash.underscore.js"
        },
        {
            "name": "nvd3",
            "location": "jam/nvd3"
        },
        {
            "name": "text",
            "location": "jam/text",
            "main": "text.js"
        },
        {
            "name": "underscore",
            "location": "jam/underscore",
            "main": "underscore.js"
        }
    ],
    "shim": {
        "d3": {
            "exports": "d3"
        }
    }
});
}
else {
    var require = {
    "packages": [
        {
            "name": "backbone",
            "location": "jam/backbone",
            "main": "backbone.js"
        },
        {
            "name": "backbone.layoutmanager",
            "location": "jam/backbone.layoutmanager",
            "main": "backbone.layoutmanager.js"
        },
        {
            "name": "bootstrap",
            "location": "jam/bootstrap"
        },
        {
            "name": "codemirror",
            "location": "jam/codemirror",
            "main": "lib/codemirror.js"
        },
        {
            "name": "d3",
            "location": "jam/d3",
            "main": "d3.js"
        },
        {
            "name": "jquery",
            "location": "jam/jquery",
            "main": "dist/jquery.js"
        },
        {
            "name": "lessc",
            "location": "jam/lessc",
            "main": "lessc.js"
        },
        {
            "name": "lodash",
            "location": "jam/lodash",
            "main": "./dist/lodash.underscore.js"
        },
        {
            "name": "nvd3",
            "location": "jam/nvd3"
        },
        {
            "name": "text",
            "location": "jam/text",
            "main": "text.js"
        },
        {
            "name": "underscore",
            "location": "jam/underscore",
            "main": "underscore.js"
        }
    ],
    "shim": {
        "d3": {
            "exports": "d3"
        }
    }
};
}

if (typeof exports !== "undefined" && typeof module !== "undefined") {
    module.exports = jam;
}