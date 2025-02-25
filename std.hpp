class String {
public:
    char* str;

    String(const char* str = nullptr);
    String(const String& other);
    ~String();
    String& operator=(const String& other);
    String& operator=(const char* str);
    String operator+(const String& other) const;
    String operator+(const char* str) const;
    String& operator+=(const String& other);
    String& operator+=(const char* str);
    bool operator==(const String& other) const;
    bool operator==(const char* str) const;
    char& operator[](int index);
    const char& operator[](int index) const;
    int size() const;
    const char* c_str() const;

    int toInt() const;
};

String::String(const char* str) {
    if (str == nullptr) {
        this->str = new char[1];
        this->str[0] = '\0';
    } else {
        int length = 0;
        while (str[length] != '\0') {
            length++;
        }
        this->str = new char[length + 1];
        for (int i = 0; i < length; i++) {
            this->str[i] = str[i];
        }
        this->str[length] = '\0';
    }
}

String::String(const String& other) {
    int length = other.size();
    this->str = new char[length + 1];
    for (int i = 0; i < length; i++) {
        this->str[i] = other.str[i];
    }
    this->str[length] = '\0';
}

String::~String() {
    delete[] this->str;
}

String& String::operator=(const String& other) {
    if (this != &other) {
        delete[] this->str;
        int length = other.size();
        this->str = new char[length + 1];
        for (int i = 0; i < length; i++) {
            this->str[i] = other.str[i];
        }
        this->str[length] = '\0';
    }
    return *this;
}

String& String::operator=(const char* str) {
    delete[] this->str;
    if (str == nullptr) {
        this->str = new char[1];
        this->str[0] = '\0';
    } else {
        int length = 0;
        while (str[length] != '\0') {
            length++;
        }
        this->str = new char[length + 1];
        for (int i = 0; i < length; i++) {
            this->str[i] = str[i];
        }
        this->str[length] = '\0';
    }
    return *this;
}

String String::operator+(const String& other) const {
    int length1 = this->size();
    int length2 = other.size();
    char* new_str = new char[length1 + length2 + 1];
    for (int i = 0; i < length1; i++) {
        new_str[i] = this->str[i];
    }
    for (int i = 0; i < length2; i++) {
        new_str[length1 + i] = other.str[i];
    }
    new_str[length1 + length2] = '\0';
    String result(new_str);
    delete[] new_str;
    return result;
}

String String::operator+(const char* str) const {
    int length1 = this->size();
    int length2 = 0;
    while (str[length2] != '\0') {
        length2++;
    }
    char* new_str = new char[length1 + length2 + 1];
    for (int i = 0; i < length1; i++) {
        new_str[i] = this->str[i];
    }
    for (int i = 0; i < length2; i++) {
        new_str[length1 + i] = str[i];
    }
    new_str[length1 + length2] = '\0';
    String result(new_str);
    delete[] new_str;
    return result;
}

String& String::operator+=(const String& other) {
    int length1 = this->size();
    int length2 = other.size();
    char* new_str = new char[length1 + length2 + 1];
    for (int i = 0; i < length1; i++) {
        new_str[i] = this->str[i];
    }
    for (int i = 0; i < length2; i++) {
        new_str[length1 + i] = other.str[i];
    }
    new_str[length1 + length2] = '\0';
    delete[] this->str;
    this->str = new_str;
    return *this;
}

String& String::operator+=(const char* str) {
    int length1 = this->size();
    int length2 = 0;
    while (str[length2] != '\0') {
        length2++;
    }
    char* new_str = new char[length1 + length2 + 1];
    for (int i = 0; i < length1; i++) {
        new_str[i] = this->str[i];
    }
    for (int i = 0; i < length2; i++) {
        new_str[length1 + i] = str[i];
    }
    new_str[length1 + length2] = '\0';
    delete[] this->str;
    this->str = new_str;
    return *this;
}

bool String::operator==(const String& other) const {
    int length1 = this->size();
    int length2 = other.size();
    if (length1 != length2) {
        return false;
    }
    for (int i = 0; i < length1; i++) {
        if (this->str[i] != other.str[i]) {
            return false;
        }
    }
    return true;
}

bool String::operator==(const char* str) const {
    int length1 = this->size();
    int length2 = 0;
    while (str[length2] != '\0') {
        length2++;
    }
    if (length1 != length2) {
        return false;
    }
    for (int i = 0; i < length1; i++) {
        if (this->str[i] != str[i]) {
            return false;
        }
    }
    return true;
}

char& String::operator[](int index) {
    return this->str[index];
}

const char& String::operator[](int index) const {
    return this->str[index];
}

int String::size() const {
    int length = 0;
    while (this->str[length] != '\0') {
        length++;
    }
    return length;
}

const char* String::c_str() const {
    return this->str;
}

int String::toInt() const {
    int result = 0;
    int length = this->size();
    for (int i = 0; i < length; i++) {
        result = result * 10 + (this->str[i] - '0');
    }
    return result;
}