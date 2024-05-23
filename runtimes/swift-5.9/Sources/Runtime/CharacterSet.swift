extension CharacterSet {
    /// Override to encode plus symbol as well
    static var urlAllowedCharacters: CharacterSet {
        var characters = CharacterSet.urlQueryAllowed
        characters.subtract(CharacterSet(charactersIn: "+"))
        return characters
    }
}