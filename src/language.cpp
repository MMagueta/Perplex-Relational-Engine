#include <map>

#include "language.hpp"

namespace Language {
  Integer::Integer(int32_t content_in) {
    content = content_in;
  }
  
  u_int32_t Integer::size(){
    return 4;
  }

  // ########################################

  Table::Table(std::map<std::string, TableAttribute> rowsIn){
    rows = rowsIn;
  };
  
  Schema::Schema(std::map<std::string, Entity> mapIn){
    map = mapIn;
  };
  
  VariableCharacters::VariableCharacters(std::string content_in) {
    content = content_in;
  }
  
  u_int32_t VariableCharacters::size(Schema schema, std::string entity_name){
    std::map<std::string, Entity>::iterator entity = schema.map.find(entity_name);
    auto x = static_cast<class Table&>(entity->second);
    x.rows
    return 1;
  }
}
