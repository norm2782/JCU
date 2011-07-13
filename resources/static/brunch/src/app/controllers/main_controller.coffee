class exports.MainController extends Backbone.Router
  routes :
    'home': 'home'

  home: =>
    $('#bd').html app.views.home.render().el
