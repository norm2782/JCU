window.app = {}
app.controllers = {}
app.models = {}
app.collections = {}
app.views = {}
app.templates = {}

# app bootstrapping on document ready
$(document).ready ->
  app.initialize = ->
    app.collections.rulesList = new RulesList()
    app.collections.rulesTree = new RulesTree()

    app.controllers.main = new MainController()
    app.views.home = new HomeView()
    app.views.rulesList = new RulesListView()
    app.views.rulesTree = new RulesTreeView()
    Backbone.history.saveLocation("home") if Backbone.history.getFragment() is ''
  app.initialize()
  Backbone.history.start()
