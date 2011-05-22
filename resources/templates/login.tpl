<apply template="layout">
  <bind tag="header"></bind>
  <div id="home-view">
    <h1>Please login</h1>
    <form method="post" action="/login">
    <table>
      <tr>
        <td>Email</td><td><input type="text" name="email" /></td>
      </tr>
      <tr>
        <td>Password</td><td><input type="password" name="password" /></td>
      </tr>
      <tr>
        <td></td>
        <td><input type="submit" value="Login" /> <input type="checkbox" value="1" name="remember" />Remember me?</td>
      </tr>
    </table>
    </form>
  </div>
</apply>
