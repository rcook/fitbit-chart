require 'json'
require_relative 'shell'

module STS
  def self.get_account_id
    obj = JSON.parse(Shell.check_capture('aws', 'sts', 'get-caller-identity'))
    obj.fetch('Account')
  end
end
