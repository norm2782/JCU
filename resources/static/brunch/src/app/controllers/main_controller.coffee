class exports.MainController extends Backbone.Controller
  routes :
    'home': 'home'

  home: =>
    $('#bd').html app.views.home.render().el
