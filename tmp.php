<?php

abstract class KeyValueStoreBase
{
	/**
	 * Integer that identifies this value store. The scope of the id depends on
	 * the id provider used.
     * @var int
	 */
	private $id;

	/** Map that stores key-value pairs */
	private Map<String, Object> values;

	/** Map that stores transient flags */
	private Map<String, Boolean> transientFlags;

	/** Construct store with set id */
	protected KeyValueStoreBase(long id) {
		this.id = id;
	}

	/** Return the id of the store */
	public long getId() {
		return id;
	}

	/** Stores a value in the {@link CloneClass} by using a keyword. */
	public void setValue(String key, Object value) {
		ensureValuesMapInitialized();
		values.put(key, value);
	}

	/** Set value using reflection to resolve types */
	public void setValue(String key, String valueString, String typeString)
			throws ClassNotFoundException, TypeConversionException {

		// Type of the value: we need to specify the classloader, since else
		// types cannot be resolved that have been loaded by the bundle class
		// loader.
		Class<?> type = ReflectionUtils.resolveType(typeString, getClass()
				.getClassLoader());

		// Make sure we really got the right type
		CCSMAssert.isTrue(type.getName().equals(typeString),
				"Type was not converted correctly.");

		// Convert value string to actual value
		Object value = ReflectionUtils.convertString(valueString, type);

		// Make sure we really got the right type
		CCSMAssert.isTrue(type.isInstance(value),
				"Value was not converted correctly.");

		setValue(key, value);
	}

	/** Gets a value */
	public Object getValue(String key) {
		if (values == null) {
			return null;
		}
		return values.get(key);
	}

	/** Checks whether a value is stored under this key */
	public boolean containsValue(String key) {
		if (values == null) {
			return false;
		}
		return values.containsKey(key);
	}

	/**
	 * Get a sorted list of the keys stored. Each key is guaranteed to only
	 * appear once. Keys are sorted in lexical order.
	 */
	public UnmodifiableList<String> getKeyList() {
		if (values == null) {
			return CollectionUtils.emptyList();
		}
		return CollectionUtils.asSortedUnmodifiableList(values.keySet());
	}

	/** Gets an int value */
	public int getInt(String key) {
		return (Integer) getValue(key);
	}

	/** Return stored long */
	public long getLong(String key) {
		return (Long) getValue(key);
	}
	
	/** Return stored long */
	public double getDouble(String key) {
		return (Double) getValue(key);
	}

	/** Gets a string value */
	public String getString(String key) {
		return (String) getValue(key);
	}

	/** Ensures that the values map is initialized */
	private void ensureValuesMapInitialized() {
		if (values == null) {
			values = new HashMap<String, Object>();
		}
	}

	/** Ensures that the values map is initialized */
	private void ensureTransientFlagsMapInitialized() {
		if (transientFlags == null) {
			transientFlags = new HashMap<String, Boolean>();
		}
	}

	/** Determines whether a value is transient. Default value is false. */
	public boolean getTransient(String key) {
		if (transientFlags == null || !transientFlags.containsKey(key)) {
			return false;
		}
		return transientFlags.get(key);
	}

	/** Stores the transient flag for a key */
	public void setTransient(String key, boolean value) {
		ensureTransientFlagsMapInitialized();
		transientFlags.put(key, value);
	}
}

class CloneClass
{

	/** The length of the clone class (number of units). */
	private int normalizedLength;

	/** A list containing all clones of a class. */
	private final Set<Clone> clones = new LinkedHashSet<Clone>();

	/**
	 * Create a new clone class with a given id
	 * 
	 * @param normalizedLength
	 *            Length of the clones (in units) in this clone class
	 */
	public CloneClass(int normalizedLength, long id) {
		super(id);
		this.normalizedLength = normalizedLength;
	}

	/**
	 * The normalized length of all clones in this class in units and thereby
	 * the length of this class.
	 */
	public int getNormalizedLength() {
		return normalizedLength;
	}

	/** Set normalized length */
	public void setNormalizedLength(int normalizedLength) {
		this.normalizedLength = normalizedLength;
	}

	/** Computes and returns the fingerprint for this clone class */
	public String getFingerprint() {
		return CloneUtils.createFingerprint(getClones());
	}

	/** Returns the clones of this clone class. */
	public UnmodifiableSet<Clone> getClones() {
		return CollectionUtils.asUnmodifiable(clones);
	}

	/** The size (number of clones) of this clone class. */
	public int size() {
		return clones.size();
	}

	/** Two clone classes are equal, if their fingerprints are equal */
	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof CloneClass)) {
			return false;
		}

		return getFingerprint().equals(((CloneClass) obj).getFingerprint());
	}

	/** Returns hash code of fingerprint */
	@Override
	public int hashCode() {
		return getFingerprint().hashCode();
	}

	/** Returns the number of gaps in the clone with the most gaps */
	public int getMaxGapNumber() {
		int gapCount = 0;
		for (Clone clone : getClones()) {
			if (gapCount < clone.gapCount()) {
				gapCount = clone.gapCount();
			}
		}
		return gapCount;
	}

	/** Returns sum of gaps contained in all clones */
	public int getGapCount() {
		int gapCount = 0;
		for (Clone clone : getClones()) {
			gapCount += clone.gapCount();
		}
		return gapCount;
	}

	/**
	 * Adds a clone to this class.
	 * <p>
	 * No sanity check is performed that makes sure that a clone really belongs
	 * to a clone class in order to allow different clone detection approaches
	 * to form clone classes for different notions of similarity
	 */
	public void add(Clone clone) {
		boolean cloneIsInOtherClass = clone.getCloneClass() != this
				&& clone.getCloneClass() != null;
		if (cloneIsInOtherClass) {
			clone.getCloneClass().remove(clone);
		}

		clones.add(clone);
		clone.setCloneClass(this);
	}

	/** Removes a clone */
	public void remove(Clone clone) {
		if (clones.remove(clone)) {
			clone.setCloneClass(null);
		}
	}
}

class MultiplexingCloneClassesCollection {

    /** Underlying collections */
    private final List<Collection<CloneClass>> collections = new ArrayList<Collection<CloneClass>>();

    /** Adds a clone class to all collections */
    public void add(CloneClass cloneClass) {
    for (Collection<CloneClass> collection : collections) {
        collection.add(cloneClass);
    }
    }

    /**
     * Returns a list with all clones in the contained collections. The list
     * is sorted by normalized length and contains no duplicates.
     */
    public List<CloneClass> getCloneClasses() {
    Set<CloneClass> resultSet = new HashSet<CloneClass>();

    for (Collection<CloneClass> boundedCollection : collections) {
        resultSet.addAll(boundedCollection);
    }

    return CollectionUtils.sort(resultSet,
        ECloneClassComparator.NORMALIZED_LENGTH);
    }

    /** Add a collection */
    public void addCollection(Collection<CloneClass> collection) {
    collections.add(collection);
    }
}

class CloneConsumer
{

    /** List in which the created clone classes are stored */
    private final MultiplexingCloneClassesCollection results = new MultiplexingCloneClassesCollection();

    /**
     * Creates a ICloneConsumer that writes the {@link CloneClass}es it
     * creates into the given set
     */
    public CloneConsumer() {
    if (top == INFINITE) {
        results.addCollection(new ArrayList<CloneClass>());
    } else {
        results.addCollection(boundedCollection(NORMALIZED_LENGTH));
        results.addCollection(boundedCollection(CARDINALITY));
        results.addCollection(boundedCollection(VOLUME));
    }
    }

    /** Creates {@link BoundedPriorityQueue} */
    private BoundedPriorityQueue<CloneClass> boundedCollection(
        ECloneClassComparator dimension) {
        return new BoundedPriorityQueue<CloneClass>(top, dimension);
    }

    /** {@link CloneClass} currently being filled */
    protected CloneClass currentCloneClass;

    /** Start new clone class */
    @Override
        public void startCloneClass(int normalizedLength) {
        currentCloneClass = new CloneClass(normalizedLength,
            idProvider.provideId());
    }

    /** Adds a clone to the current {@link CloneClass} */
    @Override
        public Clone addClone(int globalPosition, int length)
        throws ConQATException {
        // compute length of clone in lines
        Unit firstUnit = units.get(globalPosition);
        Unit lastUnit = units.get(globalPosition + length - 1);
        List<Unit> cloneUnits = units.subList(globalPosition,
            globalPosition + length);

        ITextElement element = resolveElement(firstUnit
            .getElementUniformPath());
        int startUnitIndexInElement = firstUnit.getIndexInElement();
        int endUnitIndexInElement = lastUnit.getIndexInElement();
        int lengthInUnits = endUnitIndexInElement - startUnitIndexInElement
            + 1;
        CCSMAssert.isTrue(lengthInUnits >= 0, "Negative length in units!");
        String fingerprint = createFingerprint(globalPosition, length);

        Clone clone = new Clone(idProvider.provideId(), currentCloneClass,
            createCloneLocation(element,
            firstUnit.getFilteredStartOffset(),
            lastUnit.getFilteredEndOffset()),
            startUnitIndexInElement, lengthInUnits, fingerprint);

        if (storeUnits) {
            CloneUtils.setUnits(clone, cloneUnits);
        }

        currentCloneClass.add(clone);

        return clone;
    }

    /** Creates the location for a clone. */
    private TextRegionLocation createCloneLocation(ITextElement element,
        int filteredStartOffset, int filteredEndOffset)
        throws ConQATException {
        int rawStartOffset = element
            .getUnfilteredOffset(filteredStartOffset);
        int rawEndOffset = element.getUnfilteredOffset(filteredEndOffset);
        int rawStartLine = element
            .convertUnfilteredOffsetToLine(rawStartOffset);
        int rawEndLine = element
            .convertUnfilteredOffsetToLine(rawEndOffset);

        return new TextRegionLocation(element.getLocation(),
            element.getUniformPath(), rawStartOffset, rawEndOffset,
            rawStartLine, rawEndLine);
    }

    /** Determine element for unit */
    protected ITextElement resolveElement(String elementUniformPath) {
    return uniformPathToElement.get(elementUniformPath);
    }

    /** Create fingerprint for current clone */
    protected String createFingerprint(int globalPosition, int length) {
    StringBuilder fingerprintBase = new StringBuilder();
    for (int pos = globalPosition; pos < globalPosition + length; pos++) {
        fingerprintBase.append(units.get(pos).getContent());
    }
    return Digester.createMD5Digest(fingerprintBase.toString());
    }

    /** Check constraints */
    @Override
        public boolean completeCloneClass() throws ConQATException {
        boolean constraintsSatisfied = constraints
            .allSatisfied(currentCloneClass);

        if (constraintsSatisfied) {
            results.add(currentCloneClass);
        }

        return constraintsSatisfied;
    }

    /** Return list containing all retained clone classes */
    public List<CloneClass> getCloneClasses() {
    return results.getCloneClasses();
    }
}

/** Collection that adds {@link CloneClass} to all contained collections */
private class MultiplexingCloneClassesCollection {

/** Underlying collections */
private final List<Collection<CloneClass>> collections = new ArrayList<Collection<CloneClass>>();

/** Adds a clone class to all collections */
public void add(CloneClass cloneClass) {
for (Collection<CloneClass> collection : collections) {
    collection.add(cloneClass);
}
}

/**
 * Returns a list with all clones in the contained collections. The list
 * is sorted by normalized length and contains no duplicates.
 */
public List<CloneClass> getCloneClasses() {
Set<CloneClass> resultSet = new HashSet<CloneClass>();

for (Collection<CloneClass> boundedCollection : collections) {
    resultSet.addAll(boundedCollection);
}

return CollectionUtils.sort(resultSet,
    ECloneClassComparator.NORMALIZED_LENGTH);
}

/** Add a collection */
public void addCollection(Collection<CloneClass> collection) {
collections.add(collection);
}
}
