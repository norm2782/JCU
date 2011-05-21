class exports.ProofTreeNode extends Backbone.Model
  # Available attributes:
  # term :: String
  # childTerms :: BackBone.Collection
  # proofResult :: String

  initialize: =>
    @set { childTerms: new Backbone.Collection() }

  term: =>
    @get('term')

  proofResult: =>
    @get('proofResult')

  setTerm: (tm) =>
    @set({term: tm})

  childTerms: =>
    @get('childTerms')

  setChildren: (data) =>
    childNo = data.children
    # TODO: grab either rhss or urhss from data, depending on some global setting,
    # and insert it as term in the new nodes
    if childNo > 0
      newChildren = new Array()
      for i in [0..childNo-1]
        newChildren.push(new ProofTreeNode({term: data.urhss[i] + "."}))
      @childTerms().refresh(newChildren)

  isValid: =>
    str = @term()
    if !str?
      return false

    # Token -> a word with possibly spaces in front and after
    # Term  -> Token ( Token {, Token}* ).
    token = "\\s*\\w+\\s*"
    regex = new RegExp(token + "\\(" + token + "(," + token + ")*\\)\\.\\s*$")
    valid = regex.test str
    valid && @childTerms().reduce(((acc, nd) -> nd.isValid() && acc), true)

  setProofResult: (data) =>
    @set({proofResult: data.status})
    @trigger('proof')
    res = data.children
    @childTerms().each((x) -> x.setProofResult(res.pop()))
