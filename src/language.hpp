#include <map>
#include <sys/types.h>
#include <string>

namespace Language {

  enum EntityLabel {
    Table
  };

  enum TypeLabel {
    Integer32,
    VariableCharacters
  };
  
  struct kind {
    TypeLabel type;
    uint size;
  };

  class Entity{};

  struct TableAttribute {
    u_int32_t position;
    kind type;
  };
  
  class Table: public Entity {
  public:
    std::map<std::string, TableAttribute> rows;
    Table(std::map<std::string, TableAttribute>);
  };
  
  class Schema {
  public:
    std::map<std::string, Entity> map;
    Schema(std::map<std::string, Entity>);
  };

  class Literal {
  public:
    virtual u_int32_t size();
  };

  class Integer: public Literal {
  public:
    int32_t content;
    Integer(int32_t);
    u_int32_t size();
  };

  class VariableCharacters: public Literal {
  public:
    std::string content;
    VariableCharacters(std::string);
    u_int32_t size(Schema, std::string);
  };

  
}
