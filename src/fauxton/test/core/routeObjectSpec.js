define([
       'api',
       'chai'
], function (FauxtonAPI, chai) {
  var assert = chai.assert,
      RouteObject = FauxtonAPI.RouteObject;

  describe('RouteObjects', function () {

    describe('renderWith', function () {
      var TestRouteObject, testRouteObject, mockLayout;

      beforeEach(function () {
        TestRouteObject = RouteObject.extend({
          crumbs: ['mycrumbs']
        });

        testRouteObject = new TestRouteObject();

        mockLayout = {
          called: false,
          clearBreadcumbsCalled: false,
          setBreadcumbsCalled: false,
          setTemplate: function (layout) {
            this.called = true;
          },

          clearBreadcrumbs: function () {
            this.clearBreadcumbsCalled = true;

          },

          setBreadcrumbs: function () {
            this.setBreadcumbsCalled = true;
          }
        };
      });

      it('Should set template for first render ', function () {
        testRouteObject.renderWith('the-route', mockLayout, 'args');

        assert.ok(mockLayout.called, 'setTempalte was called');
      });

      it('Should not set template after first render', function () {
        testRouteObject.renderWith('the-route', mockLayout, 'args');

        mockLayout.called = false;
        testRouteObject.renderWith('the-route', mockLayout, 'args');

        assert.notOk(mockLayout.called, 'SetTemplate not meant to be called');
      });

      it('Should clear breadcrumbs', function () {
        testRouteObject.renderWith('the-route', mockLayout, 'args');
        assert.ok(mockLayout.clearBreadcumbsCalled, 'Clear Breadcrumbs called');
      });

      it('Should set breadcrumbs', function () {
        testRouteObject.renderWith('the-route', mockLayout, 'args');
        assert.ok(mockLayout.setBreadcumbsCalled, 'Set Breadcrumbs was called');
      });
    });

  });


});
