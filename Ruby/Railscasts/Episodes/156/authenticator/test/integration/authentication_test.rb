require 'test_helper'
 
class AuthenticationTest < ActionController::IntegrationTest
  def setup
    DatabaseCleaner.clean
  end
  test "logging in with valid username and password" do
    User.create!(:username => "pjb3", :email => "paul@example.com", :password => "too_many_secrets")
    visit login_url
    fill_in "Username", :with => "pjb3"
    fill_in "Password", :with => "too_many_secrets"
    click_button "Log in"
    assert_contain "Logged in successfully."
  end
  
  test "logging in with invalid username and password" do
    User.create!(:username => "pjb3", :email => "paul@example.com", :password => "too_many_secrets")
    visit login_url
    fill_in "Username", :with => "pjb3"
    fill_in "Password", :with => "FAIL"
    click_button "Log in"
    assert_contain "Invalid login or password."
  end
end