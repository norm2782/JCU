window.app = {}
app.controllers = {}
app.models = {}
app.collections = {}
app.views = {}
app.styles = {}
app.templates = {}

RulesList = require('collections/rules_list_collection').RulesList
RulesTree = require('collections/rules_tree_collection').RulesTree
MainController = require('controllers/main_controller').MainController
HomeView = require('views/home_view').HomeView
RulesListView = require('views/rules_list_view').RulesListView
RulesTreeView = require('views/rules_tree_view').RulesTreeView

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
