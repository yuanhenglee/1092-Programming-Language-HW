#include <iostream>
#include <string>

using namespace std;

class Animal {
  string m_name;

  public:
  Animal(string n):m_name(n){}
  virtual string name(){return m_name;}
  virtual string talk(){return "wut?";};
};

class Cat : virtual public Animal {
  public:
  Cat(string n):Animal(n){}
  virtual string talk(){return "meow";};
};

class Dog: virtual public Animal {
  public:
  Dog(string n):Animal(n){}
    virtual string talk(){return "woof";};
};

class Monster : virtual public Cat, virtual public Dog {
  public:
  Monster(string n):Cat(n),Dog(n),Animal(n){}
  //virtual string talk(){
  //      return Cat::talk()+Dog::talk()+"waaaa";
  //};
};

int main()
{

  Cat c = Cat("cat");
  cout<<c.name()<<":"<<c.talk()<<endl;

  Dog d("dog");
  cout<<d.name()<<":"<<d.talk()<<endl;

  Monster m("monster");
  cout<<m.name()<<":"<<m.talk()<<endl;
  return 0;
}