typedef struct {
    Eterm module;
    Eterm name;
    enum {
        create,
        update,
        get_elements
    } code_type;

    Eterm states[ERTS_ADDRESSV_SIZE];

    /* FieldName0 Dst0 FieldName1 Dst1 ... FieldNameN DstN */
    Eterm names_and_dest[];
} ErtsExtRecordKey;

typedef struct {
    bool is_loaded;

    /* Literal-tagged CONS pointer. The head of the CONS holds to
     * pointer to the canonical ErtsRecordDefinition for each code
     * generation, while the tail holds the default values. */
    Eterm cons;
    Eterm code;
} ErtsExtRecordState;
