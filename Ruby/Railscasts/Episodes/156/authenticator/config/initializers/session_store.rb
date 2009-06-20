# Be sure to restart your server when you modify this file.

# Your secret key for verifying cookie session data integrity.
# If you change this key, all old sessions will become invalid!
# Make sure the secret is at least 30 characters and all random, 
# no regular words or you'll be exposed to dictionary attacks.
ActionController::Base.session = {
  :key         => '_authenticator_session',
  :secret      => 'db0183ee9ad479b3150b7ee1c7f9c93d71ab405966f1640299b45828e51fc1560061f4dd49a2af1a5dd8dba624ef5a61fc22be0a9f85cf675abbcdd9d816c879'
}

# Use the database for sessions instead of the cookie-based default,
# which shouldn't be used to store highly confidential information
# (create the session table with "rake db:sessions:create")
# ActionController::Base.session_store = :active_record_store
